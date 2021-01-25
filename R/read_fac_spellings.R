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

    # Add all alternative spellings
    alt_spellings <- FAC_SPELLINGS_LOC %>%
        read_csv(col_types = "dcccc") %>%
        mutate(xwalk_name_clean = clean_fac_col_txt(str_to_upper(xwalk_name_clean))) %>%
        mutate(xwalk_name_raw = clean_fac_col_txt(str_to_upper(xwalk_name_raw)))

    # Add all entries in fac_info where the clean name is the same as the alt name
    clean_spellings <- read_fac_info() %>%
        mutate(xwalk_name_raw = Name,
               xwalk_name_clean = Name) %>%
        select(Facility.ID, State, xwalk_name_raw, xwalk_name_clean, Jurisdiction)

    out <- bind_rows(alt_spellings, clean_spellings) %>%
        select(Facility.ID, State, xwalk_name_raw, xwalk_name_clean, Jurisdiction) %>%
        unique()

    return (out)

}
