#' Get general population state COVID vaccine data from CDC and Census 2020 18+ pop estimates
#'
#' @description Pulls the latest data from the CDC on COVID vaccinations initiated
#' for adults in a state and then merges alongside a population number from the 18+ 2020
#' Census population estimates for that state to use for rate calculation.
#'
#' @return a data frame with the following columns: State, Date,
#' General.Initiated, General.Adult.Population
#'
#' @examples
#'# get state-level vax data
#' st_gen_df <- get_genstate_vax()
#'
#' @importFrom jsonlite read_json
#' @export

get_genstate_vax <- function(){

    adultpop_df <- "https://www2.census.gov/programs-surveys/popest/datasets/" %>%
        stringr::str_c("2010-2020/state/asrh/sc-est2020-18+pop-res.csv") %>%
        readr::read_csv(col_types = readr::cols()) %>%
        select(State = NAME,
               General.Adult.Population = POPEST18PLUS2020) %>%
        mutate(State = ifelse(State == "United States", "National", State))

    raw_cdc_vax <- str_c(
        "https://covid.cdc.gov/covid-data-tracker/COVIDData/",
        "getAjaxData?id=vaccination_data") %>%
        jsonlite::read_json(simplifyVector = TRUE)

    gen_vax_df <- as_tibble(raw_cdc_vax$vaccination_data) %>%
        mutate(General.Initiated = Administered_Dose1_Recip_18Plus,
               General.Completed = Series_Complete_18Plus)  %>%
        mutate(State = translate_state(Location)) %>%
        mutate(State = ifelse(Location == "PR", "Puerto Rico", State),
               State = ifelse(Location == "US", "National", State)) %>%
        mutate(Date = lubridate::ymd(Date)) %>%
        left_join(adultpop_df, by = "State") %>%
        filter(!is.na(State)) %>%
        select(State, General.Initiated, General.Completed,
               General.Adult.Population, Date)

    return(gen_vax_df)
}
