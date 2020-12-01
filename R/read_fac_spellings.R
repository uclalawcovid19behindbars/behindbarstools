#' Read in UCLA Prison and Jail Facility Data
#'
#' Reads in facility alternative name spellings
#'
#' @return data frame with facility identifiers and variations on those names
#'
#' @importFrom readr read_csv
#' @export

read_fac_spellings <- function(){
    "https://raw.githubusercontent.com/uclalawcovid19behindbars" %>%
        str_c("/facility_data/master/data_sheets/fac_spellings.csv") %>%
        read_csv(col_types = cols()) %>%
        select(
            ID = Count.ID, State, 
            facility_name_clean,
            facility_name_raw) %>%
        mutate(xwalk_name_clean = clean_fac_col_txt(str_to_upper(facility_name_clean))) %>%
        mutate(xwalk_name_raw = clean_fac_col_txt(str_to_upper(facility_name_raw))) %>%
        unique() %>% 
        select(-c(facility_name_clean, facility_name_raw))
}