#' A facility name cleaning function. Uses GitHub facility name cross-walk to find all possible name variations
#'
#' A facility name cleaning function. Uses GitHub facility name cross-walk to find all possible name variations.
#' Cleans federal and non-federal facilities in separate processes, which requires a column named "jurisdiction"
#' to indicate federal/nonfederal 
#'
#' @param dat Scraped/historical data with columns Name, State, jurisdiction
#' @return data set with cleaned facility name column, "Name", from crosswalk on GitHub
#'
#'
#' @export

merge_facility_info <- function(dat){
  fac_info <- read_fac_info()

  federal_fac_info <- read_fac_info() %>%
    filter(str_detect(Jurisdiction, "(?i)federal"))

  



    return(full_df)
}