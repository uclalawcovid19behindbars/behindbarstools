#' A facility name cleaning function. Uses GitHub facility name cross-walk to find all possible name variations
#'
#' A facility name cleaning function. Uses GitHub facility name cross-walk to find all possible name variations.
#' Cleans federal and non-federal facilities in separate processes, in order to use "State" to merge or not
#'
#' @param dat Scraped/historical data with columns Name and State, at the very least
#' @return data set with cleaned facility name column, "Name", from crosswalk on GitHub
#'
#' @import stringr
#'
#' @export

clean_facility_name <- function(dat){
  name_xwalk <- read_fac_spellings()

  dat <- dat %>%
    mutate(scrape_name_clean = clean_fac_col_txt(str_to_upper(Name)))

  nonfederal <- dat %>%
    dplyr::filter_all(any_vars(!is_federal(.))) %>%
    nest_join(name_xwalk, by = c("scrape_name_clean" = "xwalk_name_raw", 
                              "State" = "State")) %>%
    hoist(name_xwalk, Name = pluck("xwalk_name_clean", 1)) %>%
    mutate(Name = map(Name, first),
          Name = as.character(Name),
          Name = ifelse(is.na(Name), scrape_name_clean, Name)) 
    
    federal_xwalk <- name_xwalk %>%
      dplyr::filter(is_federal(State))
      
    federal <- dat %>%
      dplyr::filter_all(any_vars(is_federal(.))) %>%
      select(-State) %>%
      nest_join(name_xwalk, 
                by = c("scrape_name_clean" = "xwalk_name_raw")) %>%
      hoist(name_xwalk, Name = pluck("xwalk_name_raw", 1)) %>%
      mutate(Name = map(Name, first),
            Name = as.character(Name),
            Name = ifelse(is.na(Name), scrape_name_clean, Name)) 

    full_df <- bind_rows(federal, nonfederal)
    return(full_df)
}

