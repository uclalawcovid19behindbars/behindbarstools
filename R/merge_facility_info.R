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
  all_fac_info <- read_fac_info()

  federal_fac_info <- all_fac_info %>%
    dplyr::filter_all(any_vars(is_federal(.))) 

  # this needs review
  federal <- federal_fac_info %>%
    right_join(
        dat %>%
            filter(jurisdiction == "federal") %>%
            select(-State),
        by = "Facility") %>%
    mutate(Name = ifelse(is.na(Name), Facility, Name)) %>%
    select(-Facility, -State) %>%
    left_join(all_fac_info,  by = c("Name", "ID")) %>%
    mutate(State = ifelse(is.na(State), "Not Available", State))

  nonfederal <- dat %>%
    filter(State != "Federal") %>%
    left_join(all_fac_info, by = c("Facility", "State")) %>%
    mutate(Name = ifelse(is.na(Name), Facility, Name)) %>%
    select(-Facility) %>%
    left_join(all_fac_info,  by = c("Name", "State", "ID"))
    
  full_df <- bind_rows(federal, nonfederal)

  return(full_df)
}