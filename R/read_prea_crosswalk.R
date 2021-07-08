#' Read in UCLA PREA audit crosswalk 
#'
#' Reads in crosswalk mapping UCLA facilities to info from PREA audits 
#'
#' @return data frame with crosswalk 
#'
#' @importFrom readr read_csv
#' @importFrom dplyr select
#'
#' @examples
#' read_prea_crosswalk()
#'
#' @export

read_prea_crosswalk <- function(){
    PREA_CROSSWALK_LOC %>%
        read_csv(col_types = cols(
            Facility.ID = "d",
            PREA.Audit.Date = "D",
            PREA.Report.Date = "D",
            PREA.Staff.Population = "d",
            PREA.Residents.Population = "d",
            PREA.Capacity = "d",
            PREA.Staff.Population.Type = "c",
            PREA.Residents.Population.Type = "c",
            PREA.Source = "c")) %>%
        select(Facility.ID, PREA.Audit.Date, PREA.Report.Date, 
               PREA.Staff.Population, PREA.Residents.Population, PREA.Capacity, 
               PREA.Staff.Population.Type, PREA.Residents.Population.Type, 
               PREA.Source)
}
