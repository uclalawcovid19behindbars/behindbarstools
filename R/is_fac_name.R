#' Returns TRUE if a facility name exists in the facility crosswalks
#'
#' Returns TRUE if a facility name exists in the facility crosswalks, FALSE
#' otherwise. Function is case-insensitive, and strips special characters, new line
#' indicators, and excess white space (per behindbarstools::clean_fac_col_txt).
#'
#' @param fac_name character string of the facility to plot
#' @param state character string of the facility's state
#' @param fac_info data frame with facility_info sheet
#' @param fac_spellings data frame with facility_spellings sheet
#' @param include_alt logical, whether to include alternative spellings
#'
#' @return logical, TRUE if fac_name is a valid name and FALSE otherwise
#'
#' @examples
#' is_fac_name("Los Angeles Jails", "California")
#'
#' @importFrom dplyr distinct
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select
#'
#' @export

is_fac_name <- function(
    fac_name, state, fac_info = NULL, fac_spellings = NULL, include_alt = T) {

    if (is.null(fac_info)) {
        fac_info <- read_fac_info()
    }
    if (is.null(fac_spellings)) {
        fac_spellings <- read_fac_spellings()
    }

    fac_data_names <- fac_info %>%
        select(Name, State) %>%
        distinct()

    if (! include_alt) {
        all_names <- fac_data_names
    }
    else {
        fac_spelling_names <- fac_spellings %>%
            select(xwalk_name_raw, State) %>%
            distinct() %>%
            rename(Name = xwalk_name_raw)
        all_names <- rbind(fac_data_names, fac_spelling_names)
    }

    rv <- clean_fac_col_txt(fac_name, to_upper = T) %in% (
        all_names %>%
            filter(State == state) %>%
            mutate(Name = clean_fac_col_txt(Name, to_upper = T)) %>%
            select(Name) %>%
            distinct() %>%
            unlist()
        )

    return(rv)
}
