#' Read in UCLA Prison and Jail Facility Data Crosswalk
#'
#' Reads in facility crosswalk data
#'
#' @return data frame with facility crosswalk data
#'
#' @importFrom readr read_csv
#' @export

read_fac_crosswalk <- function(){
    fac_spellings <- read_fac_spellings()
    fac_info <- read_fac_info()
    crosswalk <- left_join(fac_spellings, fac_info, 
                            by = c("xwalk_name_clean" = "Name", 
                                   "State" = "State"))
}
