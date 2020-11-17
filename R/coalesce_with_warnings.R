#' A re-coding of the coalesce function to include warnings when multiple
#' values are given which are not NA and are different
#'
#' @param ... vectors of equal length and type to coalesce
#' @return vector of coalesced values
#'
#' @examples
#' coalesce_with_warnings(c(1, 2, 3), c(4, 5, 6))
#' coalesce_with_warnings(c(1, 2, 3), c(1, 2, NA))
#'
#' @export

coalesce_with_warnings <- function(...){
    d <- cbind(...)

    sapply(1:nrow(d), function(i){
        x <- d[i,]
        if(all(is.na(x))){
            out <- NA
        }
        else{
            xbar <- unique(as.vector(na.omit(x)))
            if(length(xbar) != 1){
                warning(paste0(
                    "Row ", i, " has multiple values that do not match."))
            }
            # only grab the first one
            out <- xbar[1]
        }
        out
    })
}
