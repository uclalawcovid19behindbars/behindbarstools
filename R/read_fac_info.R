#' Read in UCLA Prison and Jail Facility Data
#'
#' Reads in facility metadata
#'
#' @return data frame with facility metadata
#'
#' @importFrom readr read_csv
#' @export

read_fac_info <- function(){
    "https://raw.githubusercontent.com/uclalawcovid19behindbars" %>%
        str_c("/facility_data/master/data_sheets/fac_data.csv") %>%
        read_csv(col_types = cols()) %>%
        select(-Jurisdiction, - Count.ID)
}
