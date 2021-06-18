#' Read in statewide population data
#'
#' Reads in statewide-aggregated anchored population data with rows for Federal,
#' ICE and 51 state agencies.
#'
#' @return data frame with population data
#'
#' @importFrom readr read_csv
#' @importFrom stringr str_c
#' @importFrom dplyr select
#'
#' @examples
#' read_aggregate_pop_data()
#'
#' @export

read_aggregate_pop_data <- function(){
    POP_ANCHOR_LOC %>%
        read_csv(col_types = cols(
            State = "c",
            Residents.Population = "d",
            Staff.Population = "d"
        )
    )
}
