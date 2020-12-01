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
    facd_df <- "https://raw.githubusercontent.com/uclalawcovid19behindbars" %>%
        str_c("/facility_data/master/data_sheets/fac_data.csv") %>%
        read_csv(col_types = cols()) %>%
        select(
            ID = Count.ID, State, Name, Address, Zipcode, City, County, 
            Latitude, Longitude, County.FIPS, hifld_id) %>%
        mutate(Name = clean_fac_col_txt(str_to_upper(Name))) %>%
        unique()
    
    facdf_df <- "https://raw.githubusercontent.com/uclalawcovid19behindbars" %>%
      str_c("/facility_data/master/data_sheets/fac_data.csv") %>%
      read_csv(col_types = cols()) %>%
      filter(str_detect(Jurisdiction, "(?i)federal")) %>%
      select(
        ID = Count.ID, State, Name, Address, Zipcode, City, County, 
        Latitude, Longitude, County.FIPS, hifld_id) %>%
      mutate(Name = clean_fac_col_txt(str_to_upper(Name))) %>%
      unique()

    return(full_df)
}