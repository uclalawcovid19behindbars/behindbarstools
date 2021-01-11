#' Merges informative facility information such as capacity, geographic information, and population.
#'
#' Merges our facility dataset with detailed descriptive information on each entity in our
#' dataset (e.g. type of entity, capacity, population, geographic information, etc.).
#' Each row represents a unique entity.
#' Data source here: https://github.com/uclalawcovid19behindbars/facility_data/data/fac_data.csv
#'
#' @param dat Scraped/historical data with column "Facility.ID"
#' @return data set with facility information columns
#'
#' @importFrom dplyr filter_all
#'
#' @examples
#' merge_facility_info(
#'     tibble(Name = "BULLOCK CORRECTIONAL FACILITY", State = "Alabama", jurisdiction = "state", Facility.ID = 7))
#'
#'
#' @export

merge_facility_info <- function(dat){
  fac_info <- read_fac_info()

  dat_with_fac_info <- dat %>%
    left_join(fac_info,
              by = "Facility.ID",
              suffix = c("", ".y")) %>%
    mutate(State = ifelse(is.na(State), State.y, State),
           Jurisdiction = ifelse(is.na(jurisdiction), Jurisdiction, jurisdiction)) %>%
    select(
      -State.y,
      -jurisdiction,
      -Name.y
    ) %>%
    relocate(Facility.ID)

  return(dat_with_fac_info)
}
