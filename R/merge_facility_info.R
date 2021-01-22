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
#'     tibble(Name = "BULLOCK CORRECTIONAL FACILITY", State = "Alabama", Jurisdiction = "state", Facility.ID = 7))
#'
#'
#' @export

merge_facility_info <- function(dat){
  fac_info <- read_fac_info()

  dat_with_fac_info <- dat %>%
    left_join(fac_info,
              by = "Facility.ID",
              suffix = c("", ".y")) %>%
    # if Name, State, or Jurisdiction are NA in data, use the values from fac_info
    # note: we NEVER expect Name, State, or Jurisdiction to be in fac_info and
    #       not fac_spellings. but in case this ever happens, the code below will
    #       populate those missing values
    mutate(
      Name = ifelse(is.na(Name), Name.y, Name),
      State = ifelse(is.na(State), State.y, State),
      Jurisdiction = ifelse(is.na(Jurisdiction), Jurisdiction.y, Jurisdiction)
      ) %>%
    # de-select Name, State, and Jurisdiction from fac_info sheet
    select(
      -Name.y,
      -State.y,
      -Jurisdiction.y,
    ) %>%
    relocate(Facility.ID)

  return(dat_with_fac_info)
}
