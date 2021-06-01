#' Alternate Aggregate UCLA data for website groupings
#'
#' Reads the UCLA aggregates counts for states for most recent
#' data within a given window and reports either state level data or national
#' data. States include values for the 50 states broken down by carceral type,
#' prison, ICE, Federal, Juvenile, Psychiatric, and county.
#'
#' @param window integer, the day range of acceptable data to pull from, ignored
#' if all dates is true
#' @param all_dates logical, get time series data rather than just latest counts
#' @param week_grouping logical, use weekly grouping for past data? else monthly
#' @param collapse_vaccine logical, combine vaccine variables for more
#' intuitive comparisons
#'
#' @return data frame with aggregated counts at state or national level
#'
#' @examples
#' \dontrun{
#' alt_aggregate_counts()
#' }
#' @export

alt_aggregate_counts <- function(
    window = 31, all_dates = FALSE, week_grouping = TRUE,
    collapse_vaccine = TRUE){

    round_ <- ifelse(week_grouping, "week", "month")

    ucla_df <- read_scrape_data(
        window = window, all_dates = all_dates, wide_data = FALSE) %>%
        mutate(Web.Group = case_when(
            Jurisdiction == "immigration" ~ "ICE",
            Jurisdiction == "federal" ~ "Federal",
            Age == "Juvenile" ~ "Juvenile",
            Jurisdiction == "state" ~ "Prison",
            Jurisdiction == "psychiatric" ~ "Psychiatric",
            Jurisdiction == "county" ~ "County",
            TRUE ~ NA_character_
        ))

    fac_long_df <- ucla_df %>%
        filter(State != "Not Available") %>%
        select(Name, Date, State, Measure, Web.Group, value)

    if(all_dates){
        state_df <- fac_long_df %>%
            mutate(Date = lubridate::floor_date(Date, round_)) %>%
            rename(UCLA = value) %>%
            filter(!is.na(UCLA)) %>%
            group_by(State, Date, Measure, Web.Group, Name) %>%
            summarize(UCLA = max_na_rm(UCLA), .groups = "drop_last") %>%
            mutate(has_statewide = "STATEWIDE" %in% Name) %>%
            # if state wide and other counts exist for a measure only take max date
            filter(!(has_statewide) | Date == max(Date)) %>%
            mutate(has_statewide = "STATEWIDE" %in% Name) %>%
            # if state wide and other counts still exist for a measure only use statewide
            filter(!(has_statewide & Name != "STATEWIDE")) %>%
            group_by(State, Date, Web.Group, Measure) %>%
            summarise(UCLA = sum_na_rm(UCLA), .groups = "drop")

        if(collapse_vaccine){
            sub_vac_res <- state_df %>%
                group_by(State, Date, Web.Group) %>%
                mutate(No.Initiated = !("Residents.Initiated" %in% Measure)) %>%
                filter(No.Initiated) %>%
                # remove vadmin in the vector if you dont want to sub for that val
                filter(Measure %in% c("Residents.Completed", "Residents.Vadmin")) %>%
                arrange(State, Web.Group, Date, Measure) %>%
                filter(1:n() == 1) %>%
                mutate(Measure = "Residents.Initiated") %>%
                ungroup()

            sub_vac_staff <- state_df %>%
                group_by(State, Date) %>%
                mutate(No.Initiated = !("Staff.Initiated" %in% Measure)) %>%
                filter(No.Initiated) %>%
                # add vadmin in the vector if you dont to sub for that val
                filter(Measure %in% c("Staff.Completed", "Staff.Vadmin")) %>%
                arrange(State, Web.Group, Date, Measure) %>%
                filter(1:n() == 1) %>%
                mutate(Measure = "Staff.Initiated") %>%
                ungroup()

            state_df <- bind_rows(state_df, sub_vac_res, sub_vac_staff) %>%
                select(-No.Initiated)
        }
    }
    else{
        state_df <- fac_long_df %>%
            rename(UCLA = value) %>%
            filter(!is.na(UCLA)) %>%
            group_by(State, Measure, Web.Group) %>%
            mutate(has_statewide = "STATEWIDE" %in% Name) %>%
            # if state wide and other counts exist for a measure only take more
            # recently scraped data
            filter(!(has_statewide) | Date == max(Date)) %>%
            mutate(has_statewide = "STATEWIDE" %in% Name) %>%
            # if state wide and other counts still exist for a measure only
            # use statewide measures
            filter(!(has_statewide & Name != "STATEWIDE")) %>%
            group_by(State, Measure, Web.Group) %>%
            summarise(
                UCLA = sum_na_rm(UCLA), Date = max(Date), .groups = "drop")

        if(collapse_vaccine){
            sub_vac_res <- state_df %>%
                group_by(State, Web.Group) %>%
                mutate(No.Initiated = !("Residents.Initiated" %in% Measure)) %>%
                filter(No.Initiated) %>%
                # add vadmin in the vector if you also want to sub for that val
                filter(Measure %in% c("Residents.Completed", "Residents.Vadmin")) %>%
                arrange(State, Measure) %>%
                filter(1:n() == 1) %>%
                mutate(Measure = "Residents.Initiated") %>%
                ungroup()

            sub_vac_staff <- state_df %>%
                group_by(State, Web.Group) %>%
                mutate(No.Initiated = !("Staff.Initiated" %in% Measure)) %>%
                filter(No.Initiated) %>%
                # add vadmin in the vector if you also want to sub for that val
                filter(Measure %in% c("Staff.Completed", "Staff.Vadmin")) %>%
                arrange(State, Measure) %>%
                filter(1:n() == 1) %>%
                mutate(Measure = "Staff.Initiated") %>%
                ungroup()

            state_df <- bind_rows(state_df, sub_vac_res, sub_vac_staff) %>%
                select(-No.Initiated)
        }
    }

    out_agg_df <- state_df %>%
        rename(Value = UCLA)

    return(out_agg_df)
}
