#' Read the latest Marshall Project State Data
#'
#' Reads the MP/AP dataset and converts variable names to be comparable to the
#' UCLA dataset
#'
#' @param all_dates return all historical data from MP/AP
#' @param window integer, the day range of acceptable data to pull from if
#' all_dates is false
#'
#' @return data frame with MP/AP results
#'
#' @examples
#' \dontrun{
#' read_mpap_data()
#' }
#' @export

read_mpap_data <- function(all_dates = FALSE, window = 14){
    mp_raw_df <- "https://raw.githubusercontent.com/themarshallproject/" %>%
        stringr::str_c(
            "COVID_prison_data/master/data/covid_prison_cases.csv") %>%
        readr::read_csv(col_types = readr::cols())

    rename_df <- mp_raw_df %>%
        mutate(Date = lubridate::mdy(as_of_date)) %>%
        select(
            State = name,
            Date,
            Residents.Confirmed = total_prisoner_cases,
            Residents.Deaths = total_prisoner_deaths,
            Residents.Tadmin = prisoner_tests_with_multiples,
            Residents.Tested = prisoner_tests,
            Residents.Recovered = prisoners_recovered,
            Staff.Confirmed = total_staff_cases,
            Staff.Deaths = total_staff_deaths,
            Staff.Tested = staff_tests,
            Staff.Recovered = staff_recovered,
            Residents.Completed = prisoners_full_dose,
            Staff.Completed = staff_full_dose
        )

    if(!all_dates){
        rename_df <- rename_df %>%
            filter(Date >= (Sys.Date() - window)) %>%
            arrange(State, Date) %>%
            group_by(State) %>%
            mutate(across(where(is.numeric), last_not_na)) %>%
            filter(Date == max(Date)) %>%
            ungroup()
    }

    return(rename_df)
}
