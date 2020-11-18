#' Read in Vera Prison population estimates
#'
#' Reads in an estimate of Vera prison population estimates
#'
#' @param updated logical, use updated population estimates from March where
#' available
#' @return data frame with population estimates
#'
#' @importFrom readxl read_excel
#' @importFrom utils download.file
#' @export

read_vera_pop <- function(updated = TRUE){
    tf <- tempfile(fileext = ".xlsx")

    "https://www.vera.org/downloads/publications/" %>%
        paste0("people-in-prison-data.xlsx") %>%
        download.file(tf, quiet = TRUE)

    null <- suppressMessages(
        prison_2019_df <- read_excel(tf, skip = 8) %>%
            dplyr::select(
                State, Dec = `December 31, 2019 prison`,
                March = `March 31 2020 prison`) %>%
            dplyr::filter(!is.na(State)) %>%
            dplyr::filter(!stringr::str_detect(State, "(?i)total")) %>%
            dplyr::filter(!stringr::str_detect(State, "(?i)states")) %>%
            # replace NA with DEC value where missing for March
            dplyr::mutate(March = ifelse(is.na(March), Dec, March)) %>%
            dplyr::mutate(Population = Dec)
    )

    if(updated){
        prison_2019_df$Population <- prison_2019_df$March
    }

    prison_2019_df %>%
        dplyr::select(State, Population)
}
