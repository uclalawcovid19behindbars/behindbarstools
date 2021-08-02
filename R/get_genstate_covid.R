#' Get general population state COVID data from CDC and Census 2020 pop estimates
#'
#' @description Pulls data from the CDC on COVID cases and deaths
#' for a state and then merges alongside a population number from the 2020
#' Census population estimates for that state to use for rate calculation.
#'
#' @return a data frame with the following columns: State, Date,
#' General.Confirmed, General.Deaths, General.Population
#'
#' @examples
#' \dontrun{
#' state_ <- "Georgia"
#'
#'# get state level covid data
#' st_gen_df <- get_genstate_covid() %>%
#'     filter(State == state_)
#'
#' # get our prison data
#' hist_df <- alt_aggregate_counts(all_dates = T)
#'
#' # get our prison denominators
#' pri_pop <- read_aggregate_pop_data() %>%
#'     filter(State == state_) %>%
#'     pull(Residents.Population) %>%
#'     first()
#'
#' # calculate new cases for the state of interest
#' hist_df %>%
#'     filter(Measure == "Residents.Confirmed" & State == state_) %>%
#'     filter(Web.Group == "Prison") %>%
#'     select(Date, Residents.Confirmed = Val) %>%
#'     mutate(Residents.New = diff_roll_sum(Residents.Confirmed, Date)) %>%
#'     mutate(Residents.Population = pri_pop) %>%
#'     # calculate the rate using latest denominators
#'     mutate(Residents = Residents.New/Residents.Population) %>%
#'     select(Date, Residents) %>%
#'     # join with the overall state data
#'     right_join(
#'         st_gen_df %>%
#'             mutate(General.New = diff_roll_sum(General.Confirmed, Date)) %>%
#'             mutate(General = General.New/General.Population) %>%
#'             select(Date, General)) %>%
#'     # pivot to make plotting easier
#'     pivot_longer(-Date, names_to = "Group", values_to = "CR") %>%
#'     filter(!is.na(CR)) %>%
#'     mutate(Group = ifelse(
#'         Group == "General", state_, str_c(state_, " DOC"))) %>%
#'     mutate(Group = fct_rev(Group)) %>%
#'     filter(Date >= lubridate::ymd("2021-01-01")) %>%
#'     ggplot(aes(x = Date, y = CR*100000, color = Group)) +
#'     geom_line(size = 2) +
#'     theme_behindbars() +
#'     scale_color_bbdiscrete() +
#'     labs(y = "New Case Rate\nPer100,000", color = "") +
#'     theme(legend.position = c(.38, .92)) +
#'     theme(legend.background = element_rect(fill = "transparent"))
#' }
#'
#' @export

get_genstate_covid <- function(){

    genpop_df <- "https://www2.census.gov/programs-surveys/popest/datasets/" %>%
        stringr::str_c("2010-2020/state/totals/nst-est2020.csv") %>%
        readr::read_csv(col_types = readr::cols()) %>%
        select(State = NAME, General.Population = POPESTIMATE2020)

    state_infections <- "https://data.cdc.gov/api/views/9mfq-cb36/rows.csv" %>%
        stringr::str_c("?accessType=DOWNLOAD") %>%
        readr::read_csv(col_types = readr::cols())

    gen_covid_df <- state_infections %>%
        rename(state_abv = state) %>%
        mutate(State = translate_state(state_abv)) %>%
        mutate(State = ifelse(state_abv == "PR", "Puerto Rico", State)) %>%
        filter(!is.na(State)) %>%
        mutate(Date = lubridate::mdy(submission_date)) %>%
        arrange(Date) %>%
        select(
            State, Date, General.Confirmed = tot_cases,
            General.Death = tot_death) %>%
        left_join(genpop_df, by = "State")

    return(gen_covid_df)
}
