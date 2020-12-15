#' Cleans character data that has been scraped from websites
#'
#' Cleans data usually associated with facilities or column names. Specifically
#' removes special characters (except spaces and dashes), capitalizes COVID, standardizes
#' COVID-19 references, removes new line indicators and excess white space,
#' and changes "state-wide" to "statewide" (case-insensitive).
#' Optionally capitalizes output.
#'
#' @param x character vector to clean
#' @param to_upper logical, convert characters to upper case
#'
#' @return cleaned character vector
#'
#' @examples
#' clean_fac_col_txt(" Messy string \n\r data   ")
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_squish
#' @importFrom stringr str_to_upper
#' @export

clean_fac_col_txt <- function(x, to_upper = FALSE){
    # get rid of excessive white space
    out <- str_squish(x) %>%
        # remove all special characters except spaces, dashes, and slashes
        str_remove_all("[^-|^[:space:]|^[:alnum:]|^/]") %>%
        # capitalize COVID wherever its found
        str_replace_all("(?i)covid", "COVID") %>%
        # replace COVID - 19 with  some form of spaces with COVID-19
        str_replace_all("COVID[ ]*-[ ]*19", "COVID-19") %>%
        # replace STATE-WIDE with STATEWIDE
        str_replace_all("(?i)state-wide", "STATEWIDE") %>%
        str_replace_all("\\n", " ") %>%
        str_replace_all("\\r", " ") %>%
        str_squish()

    if(to_upper){
        out <- str_to_upper(out)
    }

    out
}
