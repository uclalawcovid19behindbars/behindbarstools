#' A facility name cleaning function. Uses GitHub facility name cross-walk to find all possible name variations
#'
#' A facility name cleaning function. Uses GitHub facility name cross-walk to find all possible name variations.
#' Cleans federal and non-federal facilities in separate processes, in order to use "State" to merge or not.
#' If no match is found in the crosswalk, both Facility.ID and Jurisdiction will equal NA in the resulting output.
#'
#' @param dat Scraped/historical data with columns Name and State, at the very least
#' @param alt_name_xwalk Optional parameter provides an alternative facility name crosswalk
#' @param debug Boolean whether to include additional columns geneated during the merging process
#'
#' @return data set with cleaned columns, "Name", "Facility.ID", and "Jurisdiction" from fac_spellings
#'
#'
#' @import stringr
#' @importFrom tidyr hoist
#' @importFrom purrr pluck
#' @importFrom purrr map
#' @importFrom assertthat %has_name%
#' @importFrom assertthat see_if
#' @importFrom readr cols
#'
#' @examples
#' clean_facility_name(
#'     tibble(Name = "BULLOCK CORRECTIONAL FACILITY", State = "Alabama", Jurisdiction = "state"))
#'
#' clean_facility_name(
#'     tibble(Name = "LEE USP", State = "Federal", Facility = "prison"))
#'
#' @export

clean_facility_name <- function(dat, alt_name_xwalk = FALSE, debug = FALSE){
    if(alt_name_xwalk) {
      name_xwalk <- alt_name_xwalk
    } else {
      name_xwalk <- read_fac_spellings() %>%
        # don't want to keep ambiguous jurisdiction xwalk entries
        filter(!is.na(Jurisdiction))
    }
    check_jurisdiction <- see_if(dat %has_name% "Jurisdiction")
    if(check_jurisdiction == FALSE){
      dat$Jurisdiction <- NA_character_
    } else {
      dat$Jurisdiction <- dat$Jurisdiction
    }
    check_facility <- see_if(dat %has_name% "Facility")
    if(check_facility == FALSE){
      dat$Facility <- NA_character_
    } else {
      dat$Facility <- dat$Facility
    }

    dat <- dat %>%
      mutate(scrape_name_clean = clean_fac_col_txt(Name, to_upper = TRUE),
             federal_bool = case_when(
               is_federal(Jurisdiction) ~ TRUE,
               is_federal(State) ~ TRUE,
               is_federal(Facility) ~ TRUE,
               # when Jurisdiction is NA, and neither State nor Facility
               # contains "federal", federal_bool = FALSE
               TRUE ~ FALSE
      ))

    nonfederal_xwalk <- name_xwalk %>%
        filter(Jurisdiction %in% c("state", "county"))

    nonfederal <- dat %>%
        filter(!federal_bool) %>%
        select(-Jurisdiction) %>% # if Jurisdiction is NA in the data, we get jurisdiction from xwalk
        left_join(nonfederal_xwalk,
                  by = c(
                      "scrape_name_clean" = "xwalk_name_raw",
                      "State" = "State")) %>%
        mutate(Name = xwalk_name_clean) %>%
        mutate(name_match = !is.na(Name)) %>%
        mutate(Name = ifelse(is.na(Name), scrape_name_clean, Name))

    nrow_nonfederal <- nrow(nonfederal)
    if(nrow_nonfederal == 0) {nonfederal <- NULL}

    federal_xwalk <- name_xwalk %>%
        filter(Jurisdiction %in% c("federal", "immigration"))

    federal <- dat %>%
        filter(federal_bool) %>%
        select(-State,              # if State = Federal in the data, we get state from xwalk
               -Jurisdiction) %>%   # if Jurisdiction = NA in the data, we get JUR from xwalk
        left_join(
            federal_xwalk,
            by = c("scrape_name_clean" = "xwalk_name_raw")
            ) %>%
        mutate(Name = xwalk_name_clean) %>%
        mutate(name_match = !is.na(Name)) %>%
        mutate(Name = ifelse(is.na(Name), scrape_name_clean, Name))

    nrow_federal <- nrow(federal)
    if(nrow_federal == 0) {federal <- NULL}

    full_df <- bind_rows(nonfederal, federal)

    if(!debug){
      full_df <- full_df %>%
          select(
              -scrape_name_clean,
              -federal_bool,
              -xwalk_name_clean,
              -name_match,
              -Facility)
    }

    return(full_df)
}
