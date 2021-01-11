#' Read in UCLA Prison and Jail Facility Data
#'
#' Reads in facility alternative name spellings
#'
#' @return data frame with facility identifiers and variations on those names
#'
#' @importFrom readr read_csv
#' @importFrom stringr str_c
#' @importFrom stringr str_to_upper
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @export
#'
#' @examples
#' read_fac_spellings()

read_fac_spellings <- function(){
    FAC_SPELLINGS_LOC %>%
        read_csv(col_types = cols()) %>%
        select(
            Facility.ID,
            State,
            xwalk_name_clean,
            xwalk_name_raw,
            Is.Federal) %>%
        mutate(xwalk_name_clean = clean_fac_col_txt(str_to_upper(xwalk_name_clean))) %>%
        mutate(xwalk_name_raw = clean_fac_col_txt(str_to_upper(xwalk_name_raw))) %>%
        unique()
}
