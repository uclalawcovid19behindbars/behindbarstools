#' A re-coding of the max function which returns NA if all values are NA
#'
#' A re-coding of the max function which returns NA if all values are NA.
#' Normally max(na.rm=TRUE) will return -Inf if all values are NA.
#'
#' @param x numeric vector
#' @return numeric vector of length 1
#'
#' @examples
#' max_na_rm(c(1:2, NA))
#' max_na_rm(c(NA, NA))
#'
#' @export

max_na_rm <- function(x){
    if(all(is.na(x))){
        return(NA)
    }
    max(x, na.rm = TRUE)
}
