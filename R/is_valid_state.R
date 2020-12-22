#' Returns TRUE if the given state is a valid state spelling 
#' 
#' Returns TRUE if the given state is a valid state spelling given our state 
#' spelling naming conventions. Under our conventions, spellings include the 
#' 50 states spelled out (e.g. "Alabama"), "DC", "Puerto Rico", "Guam", 
#' "Virgin Islands", and "Not Available" (e.g. for federal facilities without a 
#' known state designation). Includes a parameter to return FALSE vs. NA if the 
#' given state spelling is NA. 
#'
#' @param state character string of the state to check 
#' @param na_is_na logical, whether NA should return NA (vs. FALSE)
#'
#' @return logical, TRUE if the given state is a valid state spelling 
#' @export
#'
#' @examples
#' is_valid_state("DC")
#' is_valid_state("District of Columbia")

is_valid_state <- function(state, na_is_na = T) {
    
    if (is.na(state) & na_is_na) {return(NA)}
    
    valid_states <- datasets::state.name %>% 
        append(c(
            "DC", 
            "Puerto Rico", 
            "Guam", 
            "Virgin Islands", 
            "Not Available"
        ))
    
    rv <- state %in% valid_states
    
    return(rv) 
}
