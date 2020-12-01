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
            Name = facility_name_clean,
            Name_Raw = facility_name_raw) %>%
        mutate(Name = clean_fac_col_txt(str_to_upper(Name))) %>%
        mutate(Name_Raw = clean_fac_col_txt(str_to_upper(Name_Raw))) %>%
        unique()
}