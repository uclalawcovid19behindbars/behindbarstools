#' Returns TRUE if the given state is a valid state spelling 
#' 
#' Returns TRUE if the given state is a valid state spelling given our state 
#' spelling naming conventions. Under our conventions, spellings include the 
#' 50 states spelled out (e.g. "Alabama"), "District of Columbia", the five 
#' populated U.S. territories, and "Not Available" (e.g. for federal facilities 
#' without a known state designation). Includes a parameter to return FALSE vs. NA 
#' if the given state spelling is NA. 
#'
#' @param state character string of the state to check 
#' @param na_is_na logical, whether NA should return NA (vs. FALSE)
#'
#' @return logical, TRUE if the given state is a valid state spelling 
#' @export
#'
#' @examples
#' is_valid_state("DC")
#' is_valid_state(c("DC", NA, "District of Columbia"))

is_valid_state <- function(state, na_is_na = F) {
    
    valid_states <- datasets::state.name %>% 
        append(c(
            "District of Columbia", 
            "Puerto Rico", 
            "Guam", 
            "Virgin Islands", 
            "American Samoa", 
            "Northern Mariana Islands", 
            "Not Available"
        ))
    
    return(ifelse(na_is_na & is.na(state), NA, state %in% valid_states))
}
