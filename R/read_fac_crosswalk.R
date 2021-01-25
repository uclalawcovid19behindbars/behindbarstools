#' Read in UCLA Prison and Jail Facility Data Crosswalk
#'
#' Reads in facility crosswalk data. NB:
#'
#' @return data frame with facility crosswalk data
#'
#' @importFrom readr read_csv
#' @importFrom dplyr left_join
#' @export

read_fac_crosswalk <- function(){
    fac_spellings <- read_fac_spellings()
    fac_info <- read_fac_info()
    crosswalk <- left_join(fac_spellings, fac_info,
                            by = c("Facility.ID"),
                           suffix = c("", ".y")) %>%
        mutate(
            State = ifelse(is.na(State), State.y, State),
            Jurisdiction = ifelse(is.na(Jurisdiction), Jurisdiction.y, Jurisdiction)
        ) %>%
        # de-select Name, State, and Jurisdiction from fac_info sheet
        select(
            -State.y,
            -Jurisdiction.y,
        ) %>%
        unique()
    return(crosswalk)
}
