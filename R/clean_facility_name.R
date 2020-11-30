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

clean_facility_name <- function(dat){
    name_xwalk <- "https://raw.githubusercontent.com/uclalawcovid19behindbars" %>%
        str_c("/facility_data/master/data_sheets/fac_spellings.csv") %>%
        read_csv(col_types = cols()) %>%
        select(
          ID = Count.ID, State, 
          facility_name_clean,
          facility_name_raw) %>%
        mutate(xwalk_name_clean = behindbarstools::clean_fac_col_txt(str_to_upper(facility_name_clean))) %>%
        mutate(xwalk_name_raw = behindbarstools::clean_fac_col_txt(str_to_upper(facility_name_raw))) %>%
        unique()

    # change this to "dat"
    dat <- dat %>%
      mutate(scrape_name_clean = behindbarstools::clean_fac_col_txt(str_to_upper(Name)))

    nonfederal <- dat %>%
      filter((State != "Federal") | (jurisdiction != 'federal')) %>% 
      nest_join(name_xwalk, by = c("scrape_name_clean" = "xwalk_name_raw", 
                                "State" = "State")) %>%
      hoist(name_xwalk, Name = pluck("facility_name_clean", 1)) %>%
      mutate(Name = map(Name, first),
            Name = as.character(Name),
            Name = ifelse(is.na(Name), scrape_name_clean, Name)) 
    
    federal_xwalk <- name_xwalk %>%
        filter(State == "Federal") 
      
    federal <- dat %>%
      filter(jurisdiction == "federal") %>%
      select(-State) %>%
      nest_join(name_xwalk, 
                by = c("scrape_name_clean" = "xwalk_name_raw")) %>%
      hoist(name_xwalk, Name = pluck("facility_name_clean", 1)) %>%
      mutate(Name = map(Name, first),
            Name = as.character(Name),
            Name = ifelse(is.na(Name), scrape_name_clean, Name)) 

    full_df <- bind_rows(federal, nonfederal)
    return(full_df)
}