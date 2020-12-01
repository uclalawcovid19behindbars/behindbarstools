#' Read in UCLA Prison and Jail Facility Data Crosswalk
#'
#' Reads in facility crosswalk data
#'
#' @return data frame with facility crosswalk data
#'
#' @importFrom readr read_csv
#' @export

read_fac_crosswalk <- function(){
    "https://raw.githubusercontent.com/uclalawcovid19behindbars" %>%
        str_c("/facility_data/master/data_sheets/fac_spellings.csv") %>%
        read_csv(col_types = cols()) %>%
        select(
            ID = Count.ID, State, Name = facility_name_clean,
            Name_Raw = facility_name_raw) %>%
        mutate(Name = clean_fac_col_txt(str_to_upper(Name))) %>%
        mutate(Name_Raw = clean_fac_col_txt(str_to_upper(Name_Raw))) %>%
        unique() %>%
        left_join(
            read_fac_info(),
            by = c("Name", "State")
        )
}
