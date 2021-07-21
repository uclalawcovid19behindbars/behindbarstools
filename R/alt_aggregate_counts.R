#' Alternate Aggregate UCLA data for website groupings
#'
#' Reads the UCLA aggregates counts for states for most recent
#' data after a given date cutoff and reports either state level data or national
#' data. States include values for the 50 states broken down by carceral type,
#' prison, ICE, Federal, Juvenile, Psychiatric, and county. For prisons, data
#' from the Marshall project is also incorporated.
#'
#' @param date_cutoff date, the earliest date of acceptable data to pull from, 
#' ignored if all dates is true
#' @param all_dates logical, get time series data rather than just latest counts
#' @param week_grouping logical, use weekly grouping for past data? else monthly
#'
#' @return data frame with aggregated counts at the state level by web groups
#'
#' @examples
#' \dontrun{
#' alt_aggregate_counts()
#' }
#' @export

alt_aggregate_counts <- function(
    date_cutoff = DATE_CUTOFF, all_dates = FALSE, week_grouping = TRUE){

    # How to round data when doing all dates
    round_ <- ifelse(week_grouping, "week", "month")

    # read in ucla data and do the appropriate grouping
    fac_long_df <- read_scrape_data(
        date_cutoff = date_cutoff, all_dates = all_dates, wide_data = TRUE) %>%
        assign_web_group() %>%
        # filter(State != "Not Available") %>%
        tidyr::pivot_longer(
            starts_with(c("Residents", "Staff")), names_to = "Measure") %>%
        select(
            Name, Date, State, Measure, Web.Group, value, Population.Feb20) %>%
        mutate(Rate = value/Population.Feb20)

    # pull in the comparable MP data
    mp_df <- read_mpap_data(
        all_dates = all_dates, date_cutoff = date_cutoff) %>%
        filter(State != "Federal")%>%
        tidyr::pivot_longer(
            -(State:Date), names_to = "Measure", values_to = "MP")

    mp_pop_df <- read_mpap_pop_data() %>%
        tidyr::pivot_longer(
            -State, names_to = "Group", values_to = "Population.Feb20") %>%
        mutate(Group = ifelse(
            str_starts(Group, "Staff"), "Staff", "Residents"))

    if(all_dates){
        mp_df <- mp_df %>%
            filter(!is.na(Date)) %>%
            mutate(Date = lubridate::floor_date(Date, round_)) %>%
            group_by(State, Date, Measure) %>%
            filter(MP == max_na_rm(MP)) %>%
            filter(1:n() == 1) %>%
            ungroup()
    }

    pop_threshold <- .8

    # aggregate the data together
    if(all_dates){
        state_df <- fac_long_df %>%
            mutate(Date = lubridate::floor_date(Date, round_)) %>%
            rename(UCLA = value) %>%
            # filter(!is.na(UCLA)) %>%
            group_by(
                State, Date, Measure, Web.Group, Name, Population.Feb20) %>%
            summarize(UCLA = max_na_rm(UCLA), .groups = "drop_last") %>%
            mutate(has_statewide = "STATEWIDE" %in% Name) %>%
            # if state wide and other counts exist for a measure
            # only take max date
            filter(!(has_statewide) | Date == max(Date)) %>%
            mutate(has_statewide = "STATEWIDE" %in% Name) %>%
            # if state wide and other counts still exist for a measure
            # only use statewide
            filter(!(has_statewide & Name != "STATEWIDE")) %>%
            group_by(State, Date, Web.Group, Measure) %>%
            mutate(rem_thresh =
                       mean(!is.na(Population.Feb20)) < pop_threshold) %>%
            mutate(Population.Feb20 = ifelse(
                rem_thresh, NA, Population.Feb20)) %>%
            select(-rem_thresh) %>%
            mutate(Rate = UCLA/Population.Feb20) %>%
            summarise(
                UCLA = sum_na_rm(UCLA),
                # Rate = sum_na_rm(Rate*Population.Feb20)/
                #     sum_na_rm(Population.Feb20),
                Rate = sum_na_rm(UCLA)/sum_na_rm(Population.Feb20),
                Date = max(Date), .groups = "drop") %>%
            mutate(Rate = ifelse(str_starts(Measure, "Staff"), NA, Rate)) %>%
            filter(!str_ends(Measure, "Population"))

        pri_df <- state_df %>%
            filter(Web.Group == "Prison") %>%
            full_join(mp_df, by = c("State", "Date", "Measure"))

    }else{
        state_df <- fac_long_df %>%
            rename(UCLA = value) %>%
            # filter(!is.na(UCLA)) %>%
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
            mutate(rem_thresh =
                       mean(!is.na(Population.Feb20)) < pop_threshold) %>%
            mutate(Population.Feb20 = ifelse(
                rem_thresh, NA, Population.Feb20)) %>%
            select(-rem_thresh) %>%
            summarise(
                UCLA = sum_na_rm(UCLA),
                # Rate = sum_na_rm(Rate*Population.Feb20)/
                #     sum_na_rm(Population.Feb20),
                Rate = sum_na_rm(UCLA)/sum_na_rm(Population.Feb20),
                Date = max(Date), .groups = "drop") %>%
            mutate(Rate = ifelse(str_starts(Measure, "Staff"), NA, Rate)) %>%
            filter(!str_ends(Measure, "Population"))

        pri_df <- state_df %>%
            filter(Web.Group == "Prison") %>%
            full_join(select(mp_df, -Date), by = c("State", "Measure"))
    }

    # combine MP and UCLA data
    out_agg_df <- pri_df %>%
        mutate(Group = ifelse(
            str_starts(Measure, "Staff"), "Staff", "Residents")) %>%
        left_join(mp_pop_df, by = c("State", "Group")) %>%
        select(-Group) %>%
        mutate(Val = case_when(
            is.na(UCLA) & is.na(MP) ~ NA_real_,
            is.na(UCLA) ~ MP,
            is.na(MP) ~ UCLA,
            UCLA >= MP ~ UCLA,
            TRUE ~ MP
        )) %>%
        mutate(Rate = ifelse(is.na(Rate), UCLA/Population.Feb20, Rate)) %>%
        mutate(
            Rate = case_when(
                is.na(UCLA) & is.na(MP) ~ NA_real_,
                is.na(UCLA) ~ MP/Population.Feb20,
                is.na(MP) ~ Rate,
                UCLA >= MP ~ Rate,
                TRUE ~ MP/Population.Feb20
            )
        ) %>%
        select(-MP, -UCLA, -Population.Feb20) %>%
        bind_rows(
            state_df %>%
                filter(Web.Group != "Prison") %>%
                rename(Val = UCLA)
        )

    return(out_agg_df)
}
