#' Cleans character data that has been scraped from websites
#'
#' Cleans data usually associated with facilities or column names. Specifically
#' removes leading and trailing asterisks capitalizes COVID, standardizes
#' COVID-19 references removes new line indicators and excess white space.
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
#' @import magrittr
#' @import stringr
#' @export

clean_fac_col_txt <- function(x, to_upper = FALSE){
    # get rid of excessive white space
    out <- stringr::str_squish(x) %>%
        # remove all special characters
        stringr::str_remove("[^[:alnum:]]") %>%
        # capitalize COVID wherever its found
        stringr::str_replace_all("(?i)covid", "COVID") %>%
        # replace COVID - 19 with  some form of spaces with COVID-19
        stringr::str_replace_all("COVID[ ]*-[ ]*19", "COVID-19") %>%
        stringr::str_replace_all("\\n", " ") %>%
        stringr::str_replace_all("\\r", " ") %>%
        stringr::str_squish()

    if(to_upper){
        out <- stringr::str_to_upper(out)
    }

    out
}
