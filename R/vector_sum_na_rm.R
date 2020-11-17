#' A re-coding of the sum function which returns NA if all values are NA
#'
#' A re-coding of the sum function which returns NA if all values are NA.
#' This function sums vectors together.
#'
#' @param ... numeric vectors of equal length
#' @return numeric vector of length 1
#'
#' @examples
#' sum_na_rm(c(1:2, NA))
#' sum_na_rm(c(NA, NA))
#'
#' @export

vector_sum_na_rm <- function(...){
    d <- rbind(...)
    apply(rbind(...), 2, function(x){
        if(all(is.na(x))){
            NA
        }
        sum(x, na.rm = TRUE)
    })
}
