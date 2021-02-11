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
#'
#' @return data frame with aggregated counts at state or national level
#'
#' @examples
#' \dontrun{
#' calc_aggregate_counts()
#' }
#' @export

calc_aggregate_counts <- function(
    window = 31, ucla_only = FALSE, state = FALSE){
    mp_data <- read_mpap_data(window = window) %>%
        select(-Date) %>%
        tidyr::pivot_longer(-State, names_to = "Measure", values_to = "MP")

    if(ucla_only){
        mp_data$MP <- NA_real_
    }

    ucla_df <- read_scrape_data(window = window)

    state_df <- ucla_df %>%
        mutate(State = ifelse(Jurisdiction == "federal", "Federal", State)) %>%
        filter(Jurisdiction %in% c("state", "federal")) %>%
        select(Name, State, starts_with("Residents"), starts_with("Staff")) %>%
        select(-Residents.Population) %>%
        tidyr::pivot_longer(
            -(Name:State), names_to = "Measure", values_to = "UCLA") %>%
        filter(!is.na(UCLA)) %>%
        group_by(State, Measure) %>%
        mutate(has_statewide = "STATEWIDE" %in% Name) %>%
        # if state wide and other counts exist for a measure only use statewide
        filter(!(has_statewide & Name != "STATEWIDE")) %>%
        group_by(State, Measure) %>%
        summarise(UCLA = sum_na_rm(UCLA), .groups = "drop") %>%
        full_join(mp_data, by = c("State", "Measure")) %>%
        mutate(Val = case_when(
            is.na(UCLA) & is.na(MP) ~ NA_real_,
            is.na(UCLA) ~ MP,
            is.na(MP) ~ UCLA,
            UCLA >= MP ~ UCLA,
            TRUE ~ MP
        ))

    if(state){
        return(state_df)
    }

    agg_df <- state_df %>%
        group_by(Measure) %>%
        summarize(
            Count = sum_na_rm(Val), Reporting = sum(!is.na(Val)),
            .groups = "drop")

    return(agg_df)
}

