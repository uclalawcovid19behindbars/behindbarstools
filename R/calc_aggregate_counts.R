max_na_rm <- function(x){
    if(all(is.na(x))){
        return(NA)
    }
    max(x, na.rm = TRUE)
}

#' Aggregate UCLA and MP data to get a more recent accurate count of COVID variables
#'
#' Reads the UCLA and MP/AP dataset aggregates counts for states for most recent
#' data within a given window and reports either state level data or national
#' data. States include values for the 50 states, Federal for BOP prisons, and
#' District of Columbia for prison in the capitol. If both UCLA and MP report a
#' value for a state the larger value for is taken.
#'
#' @param window integer, the day range of acceptable data to pull from
#' @param ucla_only logical, only consider data from UCLA
#' @param state logical, return state level data
#' @param collapse_vaccine logical, combine vaccine variables for more
#' intuitive comparisons
#' @param all_dates logical, get time series data rather than just latest counts
#' @param week_grouping logical, use weekly grouping for past data? else monthly
#'
#' @return data frame with aggregated counts at state or national level
#'
#' @examples
#' \dontrun{
#' calc_aggregate_counts()
#' }
#' @export

calc_aggregate_counts <- function(
    window = 31, ucla_only = FALSE, state = FALSE, collapse_vaccine = TRUE,
    all_dates = FALSE, week_grouping = TRUE){

    round_ <- ifelse(week_grouping, "week", "month")

    to_report <- c(datasets::state.name, "Federal", "ICE")

    mp_data_wide <- read_mpap_data(window = window, all_dates = all_dates)

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

    ucla_df <- read_scrape_data(window = window, all_dates = all_dates)

    state_wide_df <- ucla_df %>%
        mutate(State = ifelse(Jurisdiction == "federal", "Federal", State)) %>%
        mutate(State = ifelse(Jurisdiction == "immigration", "ICE", State)) %>%
        filter(Jurisdiction %in% c("state", "federal", "immigration")) %>%
        select(
            Name, Date, State,
            starts_with("Residents"), starts_with("Staff")) %>%
        select(-Residents.Population) %>%
        `if`(
            collapse_vaccine,
            mutate(., Staff.Vadmin = ifelse(
                is.na(.$Staff.Vadmin), .$Staff.Initiated, .$Staff.Vadmin)),
            .) %>%
        `if`(
            collapse_vaccine,
            mutate(., Residents.Vadmin = ifelse(
                is.na(.$Residents.Vadmin), .$Residents.Initiated, .$Residents.Vadmin)),
            .) %>%
        `if`(
            collapse_vaccine,
            mutate(., Staff.Vadmin = ifelse(
                is.na(.$Staff.Vadmin), .$Staff.Completed, .$Staff.Vadmin)),
            .) %>%
        `if`(
            collapse_vaccine,
            mutate(., Residents.Vadmin = ifelse(
                is.na(.$Residents.Vadmin), .$Residents.Completed, .$Residents.Vadmin)),
            .)

    if(all_dates){
        state_df <- state_wide_df %>%
            mutate(Date = lubridate::floor_date(Date, round_)) %>%
            tidyr::pivot_longer(
                -(Name:State), names_to = "Measure", values_to = "UCLA") %>%
            filter(!is.na(UCLA)) %>%
            group_by(State, Date, Measure, Name) %>%
            summarize(UCLA = max_na_rm(UCLA), .groups = "drop_last") %>%
            mutate(has_statewide = "STATEWIDE" %in% Name) %>%
            # if state wide and other counts exist for a measure only use statewide
            filter(!(has_statewide & Name != "STATEWIDE")) %>%
            group_by(State, Date, Measure) %>%
            summarise(UCLA = sum_na_rm(UCLA), .groups = "drop")

        comb_df <- state_df %>%
            full_join(mp_data, by = c("State", "Measure", "Date"))
    }
    else{
        state_df <- state_wide_df %>%
            tidyr::pivot_longer(
                -(Name:State), names_to = "Measure", values_to = "UCLA") %>%
            filter(!is.na(UCLA)) %>%
            group_by(State, Measure) %>%
            mutate(has_statewide = "STATEWIDE" %in% Name) %>%
            # if state wide and other counts exist for a measure only use statewide
            filter(!(has_statewide & Name != "STATEWIDE")) %>%
            group_by(State, Measure) %>%
            summarise(
                UCLA = sum_na_rm(UCLA), Date = max(Date), .groups = "drop")

        comb_df <- state_df %>%
            rename(Date.UCLA = Date) %>%
            full_join(
                rename(mp_data, Date.MP = Date), by = c("State", "Measure"))
    }

    harm_df <- comb_df %>%
        mutate(Val = case_when(
            is.na(UCLA) & is.na(MP) ~ NA_real_,
            is.na(UCLA) ~ MP,
            is.na(MP) ~ UCLA,
            UCLA >= MP ~ UCLA,
            TRUE ~ MP
        ))

    if(state){
        return(harm_df)
    }

    agg_df <- harm_df %>%
        filter(!is.na(Val)) %>%
        group_by(Measure)

    if(all_dates){
        agg_df <- group_by(agg_df, Date, Measure)
    }

    out_agg_df <- agg_df %>%
        summarize(
            Count = sum_na_rm(Val), Reporting = sum(!is.na(Val)),
            Missing = paste0(
                to_report[!(to_report %in% State)], collapse = ", "),
            .groups = "drop")

    return(out_agg_df)
}
