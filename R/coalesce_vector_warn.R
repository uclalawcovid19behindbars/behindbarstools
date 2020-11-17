#' A re-coding of the coalesce_vector function to include warnings when multiple
#' values are given which are not NA and are different
#'
#' @param x vector to coalesce
#' @return vector of length 1 of coalesced values
#'
#' @examples
#' coalesce_vector_warn(c(1,2))
#'
#' @importFrom stats na.fail
#' @export

coalesce_vector_warn <- function(x) {
    if(all(is.na(x))){
        out <- NA
    }
    else{
        xbar <- unique(as.vector(na.omit(x)))
        if(length(xbar) != 1){
            warning(paste0("x has multiple values that do not match."))
        }
        # only grab the first one
        out <- xbar[1]
    }
    out
}
