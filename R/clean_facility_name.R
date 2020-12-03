#' A facility name cleaning function. Uses GitHub facility name cross-walk to find all possible name variations
#'
#' A facility name cleaning function. Uses GitHub facility name cross-walk to find all possible name variations.
#' Cleans federal and non-federal facilities in separate processes, in order to use "State" to merge or not
#'
#' @param dat Scraped/historical data with columns Name and State, at the very least
#' @param alt_name_xwalk Optional parameter provides an alternative facility name crosswalk
#'
#' @return data set with cleaned facility name column, "Name", from crosswalk on GitHub
#'
#' @import stringr
#' @importFrom tidyr hoist
#' @importFrom purrr pluck
#' @importFrom purrr map
#' @importFrom assertthat has_name
#' @importFrom assertthat see_ifd
#'
#' @export

clean_facility_name <- function(dat, alt_name_xwalk = FALSE){
  if(alt_name_xwalk) {
    name_xwalk <- alt_name_xwalk
  }
  else {
    name_xwalk <- read_fac_spellings()
  }
  check_jurisdiction <- see_if(dat %has_name% "jurisdiction")
  if(check_jurisdiction == FALSE){
    dat$jurisdiction <- NA
  } else {
    dat$jurisdiction <- dat$jurisdiction
  }

  dat <- dat %>%
    mutate(scrape_name_clean = clean_fac_col_txt(Name, to_upper = TRUE),
           federal_bool = case_when(
             is_federal(jurisdiction) ~ TRUE,
             is_federal(State) ~ TRUE,
             is_federal(Facility) ~ TRUE,
             TRUE ~ FALSE
           ))

  nonfederal <- dat %>%
    filter(federal_bool == FALSE) %>%
    nest_join(name_xwalk, by = c("scrape_name_clean" = "xwalk_name_raw",
                              "State" = "State")) %>%
    hoist(name_xwalk, Name = pluck("xwalk_name_clean", 1)) %>%
    mutate(Name = map(Name, first),
          Name = as.character(Name),
          Name = ifelse(is.na(Name), scrape_name_clean, Name))

  nrow_nonfederal <- nrow(nonfederal)
  if(nrow_nonfederal == 0) {nonfederal <- NULL}

  federal_xwalk <- name_xwalk %>%
    dplyr::filter(is_federal(State))

  federal <- dat %>%
    filter(federal_bool == TRUE) %>%
    select(-State) %>%
    nest_join(name_xwalk,
              by = c("scrape_name_clean" = "xwalk_name_raw")) %>%
    hoist(name_xwalk, Name = pluck("xwalk_name_raw", 1)) %>%
    mutate(Name = map(Name, first),
          Name = as.character(Name),
          Name = ifelse(is.na(Name), scrape_name_clean, Name))
  nrow_federal <- nrow(federal)
  if(nrow_federal == 0) {federal <- NULL}

  full_df <- bind_rows(federal, nonfederal)
  return(full_df)
}

