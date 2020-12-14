#' Read in UCLA Population Data with cleaned names
#'
#' Reads in population data
#'
#' @return data frame with population data
#'
#' @importFrom readr read_csv
#' @export
#'
#' @examples
#' read_pop_data()

read_pop_data <- function(){
    "https://raw.githubusercontent.com/uclalawcovid19behindbars/Population/" %>%
        str_c("main/initial/Merg_Pop.csv") %>%
        read_csv(col_types = cols()) %>%
        rename(Name_Raw = Name) %>%
        select(Name_Raw, Population, State) %>%
        mutate(Name_Raw = str_remove(Name_Raw, "ASPC ")) %>%
        mutate(Name_Raw = clean_fac_col_txt(str_to_upper(Name_Raw))) %>%
        # there are some duplicate names in here that shouldnt be happening
        distinct(Name_Raw, State, .keep_all = TRUE) %>%
        left_join(
            read_fac_crosswalk() %>%
                select(Name= xwalk_name_clean, Name_Raw = xwalk_name_raw, State),
            by = c("Name_Raw", "State")) %>%
        mutate(Name = ifelse(is.na(Name), Name_Raw, Name)) %>%
        select(-Name_Raw)
}
