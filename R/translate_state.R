#' Converts state abbreviations to state names
#'
#' Converts state abbreviations to state names or does the reverse
#'
#' @param x character vector of state abbreviations or names
#' @param reverse logical, reverse translation from full name to abbreviation
#'
#' @return character vector of state names
#'
#' @examples
#' translate_state(c("CA", "NM", "DC"))
#'
#' @export

translate_state <- function(x, reverse = FALSE){
    state_vec <- c(datasets::state.name, "District of Columbia", "Federal")
    names(state_vec) <- c(datasets::state.abb, "DC", "federal")

    if(reverse){
        state_vec <- c(datasets::state.abb, "DC", "Federal")
        names(state_vec) <- c(datasets::state.name, "District of Columbia", "federal")
    }

    state_vec[x]
}
