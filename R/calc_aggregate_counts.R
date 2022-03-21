#' Aggregate UCLA and MP data to get a more recent accurate count of COVID variables
#'
#' Reads the UCLA and MP/AP dataset aggregates counts for states for most recent
#' data after a given date cutoff and reports either state level data or national
#' data. States include values for the 50 state DOCs, Federal for BOP prisons,
#' ICE detention centers, and incarcerated individuals under the administration
#' of the District of Columbia DOC. If both UCLA and MP report a
#' value for a state the larger value for is taken.
#'
#' @param date_cutoff date, the earliest date of acceptable data to pull from
#' if all_dates is FALSE for .Confirmed and .Deaths variables
#' @param window integer, the day range of acceptable UCLA data to pull from
#' if all_dates is FALSE for all variables EXCEPT .Confirmed and .Deaths
#' @param ucla_only logical, only consider data from UCLA
#' @param state logical, return state level data
#' @param collapse_vaccine logical, combine vaccine variables for more
#' intuitive comparisons
#' @param all_dates logical, get time series data rather than just latest counts
#' @param week_grouping logical, use weekly grouping for past data? else monthly
#' @param only_prison logical, whether to only include Prison, Federal, and ICE
#' web groups (state prisons, federal prisons, and ICE detention)
#'
#' @return data frame with aggregated counts at state or national level
#'
#' @examples
#' \dontrun{
#' calc_aggregate_counts()
#' }
#' calc_aggregate_counts(state = TRUE, all_dates = TRUE)
#' @export

calc_aggregate_counts <- function(
    date_cutoff = DATE_CUTOFF, window = 31, ucla_only = FALSE, state = FALSE,
    collapse_vaccine = TRUE, all_dates = FALSE, week_grouping = TRUE,
    only_prison = TRUE){

    round_ <- ifelse(week_grouping, "week", "month")

    to_report <- c(
        datasets::state.name, "Federal", "ICE", "District of Columbia")

    mp_data_wide <- read_mpap_data(date_cutoff = date_cutoff, all_dates = all_dates)

    if(all_dates){
        mp_data <- mp_data_wide %>%
            filter(!is.na(Date)) %>%
            mutate(Date = lubridate::floor_date(Date, round_)) %>%
            tidyr::pivot_longer(
                -(State:Date), names_to = "Measure", values_to = "MP") %>%
            group_by(State, Date, Measure) %>%
            summarize(MP = max_na_rm(MP), .groups = "drop")
    }
    else{
        mp_data <- mp_data_wide %>%
            tidyr::pivot_longer(
            -(State:Date), names_to = "Measure", values_to = "MP")
    }

    if(ucla_only){
        mp_data$MP <- NA_real_
    }

    ucla_df <- read_scrape_data(
        date_cutoff = date_cutoff, window = window, all_dates = all_dates, wide_data = FALSE)

    fac_long_df <- ucla_df %>%
        mutate(State = ifelse(Jurisdiction == "federal", "Federal", State)) %>%
        mutate(State = ifelse(Jurisdiction == "immigration", "ICE", State)) %>%
        ## filter out juvenile, psychiatric, and most county facilities
        filter(
            Web.Group %in% c("Prison", "Federal", "ICE") |
                (State == "District of Columbia" & Jurisdiction == "county")) %>%
        select(Name, Date, State, Measure, value)

    if(all_dates){
        state_df <- fac_long_df %>%
            mutate(Date = lubridate::floor_date(Date, round_)) %>%
            rename(UCLA = value) %>%
            filter(!is.na(UCLA)) %>%
            group_by(State, Date, Measure, Name) %>%
            summarize(UCLA = max_na_rm(UCLA), .groups = "drop_last") %>%
            mutate(has_statewide = "STATEWIDE" %in% Name) %>%
            # if state wide and other counts exist for a measure only take max date
            filter(!(has_statewide) | Date == max(Date)) %>%
            mutate(has_statewide = "STATEWIDE" %in% Name) %>%
            # if state wide and other counts still exist for a measure only use statewide
            filter(!(has_statewide & Name != "STATEWIDE")) %>%
            group_by(State, Date, Measure) %>%
            summarise(UCLA = sum_na_rm(UCLA), .groups = "drop")

        if(collapse_vaccine){
            sub_vac_res <- state_df %>%
                group_by(State, Date) %>%
                mutate(No.Initiated = !("Residents.Initiated" %in% Measure)) %>%
                filter(No.Initiated) %>%
                # remove vadmin in the vector if you dont want to sub for that val
                filter(Measure %in% c("Residents.Completed")) %>%
                arrange(State, Date, Measure) %>%
                filter(1:n() == 1) %>%
                mutate(Measure = "Residents.Initiated") %>%
                ungroup()

            sub_vac_staff <- state_df %>%
                group_by(State, Date) %>%
                mutate(No.Initiated = !("Staff.Initiated" %in% Measure)) %>%
                filter(No.Initiated) %>%
                # add vadmin in the vector if you dont to sub for that val
                filter(Measure %in% c("Staff.Completed")) %>%
                arrange(State, Date, Measure) %>%
                filter(1:n() == 1) %>%
                mutate(Measure = "Staff.Initiated") %>%
                ungroup()

            state_df <- bind_rows(state_df, sub_vac_res, sub_vac_staff) %>%
                select(-No.Initiated)
        }

        comb_df <- state_df %>%
            full_join(mp_data, by = c("State", "Measure", "Date")) %>%
            arrange(State, Date, Measure)
    }
    else{
        state_df <- fac_long_df %>%
            rename(UCLA = value) %>%
            filter(!is.na(UCLA)) %>%
            group_by(State, Measure) %>%
            mutate(has_statewide = "STATEWIDE" %in% Name) %>%
            # if state wide and other counts exist for a measure only take more
            # recently scraped data
            filter(!(has_statewide) | Date == max(Date)) %>%
            mutate(has_statewide = "STATEWIDE" %in% Name) %>%
            # if state wide and other counts still exist for a measure only
            # use statewide measures
            filter(!(has_statewide & Name != "STATEWIDE")) %>%
            # # if vaccine pct exists and vaccine pct is NOT statewide, don't sum it
            mutate(UCLA = ifelse(str_detect(Measure, ".Pct") & !(has_statewide),
                                 NA, UCLA)) %>%
            filter(!is.na(UCLA)) %>%
            group_by(State, Measure) %>%
            summarise(
                UCLA = sum_na_rm(UCLA), Date = max(Date), .groups = "drop")

        if(collapse_vaccine){
            sub_vac_res <- state_df %>%
                group_by(State) %>%
                mutate(No.Initiated = !("Residents.Initiated" %in% Measure)) %>%
                filter(No.Initiated) %>%
                # add vadmin in the vector if you also want to sub for that val
                filter(Measure %in% c("Residents.Completed")) %>%
                arrange(State, Measure) %>%
                filter(1:n() == 1) %>%
                mutate(Measure = "Residents.Initiated") %>%
                ungroup()

            sub_vac_staff <- state_df %>%
                group_by(State) %>%
                mutate(No.Initiated = !("Staff.Initiated" %in% Measure)) %>%
                filter(No.Initiated) %>%
                # add vadmin in the vector if you also want to sub for that val
                filter(Measure %in% c("Staff.Completed")) %>%
                arrange(State, Measure) %>%
                filter(1:n() == 1) %>%
                mutate(Measure = "Staff.Initiated") %>%
                ungroup()

            state_df <- bind_rows(state_df, sub_vac_res, sub_vac_staff) %>%
                select(-No.Initiated)
        }

        comb_df <- state_df %>%
            rename(Date.UCLA = Date) %>%
            full_join(
                rename(mp_data, Date.MP = Date), by = c("State", "Measure")) %>%
            arrange(State, Measure)
    }

    harm_df <- comb_df %>%
        mutate(Val = case_when(
            is.na(UCLA) & is.na(MP) ~ NA_real_,
            is.na(UCLA) ~ MP,
            is.na(MP) ~ UCLA,
            UCLA >= MP ~ UCLA,
            TRUE ~ MP
        ))

    # Join with anchored population data
    if(state){
        aggregate_pop_df <- read_aggregate_pop_data()

        out_state_df <- harm_df %>%
            left_join(select(aggregate_pop_df, -Date) , by = "State") %>%
            mutate(Pop.Anchor = case_when(
                str_detect(Measure, "Residents.") ~ Residents.Population,
                str_detect(Measure, "Staff.") ~ Staff.Population)) %>%
            select(-Residents.Population, -Staff.Population)

        return(out_state_df)
    }

    agg_df <- harm_df %>%
        filter(!is.na(Val)) %>%
        group_by(Measure)

    if(all_dates){
        agg_df <- group_by(agg_df, Date, Measure)
    }

    UCLA_reporting_and_missing <- agg_df %>%
        select(-Date.MP, -MP, -Val) %>%
        filter(!is.na(UCLA)) %>%
        summarize(
            Reporting = sum(!is.na(UCLA)),
            Missing = paste0(
                to_report[!(to_report %in% State)], collapse = ", "))

    # Aggregate by Val, including MP, then add the reporting and missing
    out_agg_df <- agg_df %>%
        summarize(Count = sum_na_rm(Val)) %>%
        left_join(UCLA_reporting_and_missing)


    return(out_agg_df)
}
