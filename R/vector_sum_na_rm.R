#' A re-coding of the sum function which returns NA if all values are NA
#'
#' A re-coding of the sum function which returns NA if all values are NA.
#' This function sums vectors together.
#'
#' @param ... numeric vectors of equal length
#' @return numeric vector of length equal to provided vectors
#'
#' @examples
#' tibble(x = c(NA, NA, NA), y = c(NA, 2, NA), z = c(1, NA, NA)) %>%
#'    mutate(new = vector_sum_na_rm(x, y, z))
#' @export

vector_sum_na_rm <- function(...){
    d <- rbind(...)
    apply(rbind(...), 2, function(x){
        sum_na_rm(x)
    })
}
