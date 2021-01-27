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
    FAC_DATA_LOC %>%
        read_csv(col_types = "dccccccdccdddccccccdcccc") %>%
        select(Facility.ID, State, Name, Jurisdiction, Description, Security,
               Age, Gender, Is.Different.Operator, Different.Operator, Population.Feb20,
               Capacity, HIFLD.ID, BJS.ID, Source.Population.Feb20, Source.Capacity, Address,
               City, Zipcode, Latitude, Longitude, County, County.FIPS, Website) %>%
        `if`(
            federal_only,
            filter(., stringr::str_detect(.$Jurisdiction, "(?i)federal")),
            .)
}
