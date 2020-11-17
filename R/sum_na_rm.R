#' A re-coding of the sum function which returns NA if all values are NA
#'
#' A re-coding of the sum function which returns NA if all values are NA.
#' Normally sum(na.rm=TRUE) will return 0 if all values are NA.
#'
#' @param x numeric vector
#' @return numeric vector of length 1
#'
#' @examples
#' sum_na_rm(c(1:2, NA))
#' sum_na_rm(c(NA, NA))
#'
#' @export

sum_na_rm <- function(x){
    if(all(is.na(x))){
        return(NA)
    }
    sum(x, na.rm = TRUE)
}
