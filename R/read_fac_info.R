#' Read in UCLA Prison and Jail Facility Data
#'
#' Reads in facility metadata
#'
#' @param federal_only filter to only federal facilities
#' @return data frame with facility metadata
#'
#' @importFrom readr read_csv
#' @importFrom stringr str_c
#' @importFrom dplyr select
#'
#' @examples
#' read_fac_info(federal_only = TRUE)
#'
#' @export

read_fac_info <- function(federal_only = FALSE){
    "https://raw.githubusercontent.com/uclalawcovid19behindbars" %>%
        str_c("/facility_data/master/data_sheets/fac_data.csv") %>%
        read_csv(col_types = cols()) %>%
        `if`(
            federal_only,
            filter(., stringr::str_detect(.$Jurisdiction, "(?i)federal")),
            .) %>%
        select(-Jurisdiction)
}
