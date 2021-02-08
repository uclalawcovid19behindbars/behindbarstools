#' Replaces NA in a vector with the previous most non NA value
#'
#' Replaces NA in a vector with the previous most non NA value. If no preceding
#' non NA values then keep as NA.
#'
#' @param x vector
#'
#' @return vector with NAs replaced with preceding value
#'
#' @examples
#' last_not_na(c(NA,1, NA, 1:3, NA))
#'
#' @export

last_not_na <- function(x){
    if(all(!is.na(x)) | (length(x) == 1)){
        return(x)
    }

    z <- x

    for(i in 2:length(x)){
        if(is.na(x[i])){
            for(j in (i-1):1){
                if(!is.na(x[j])){
                    z[i] <- x[j]
                    break
                }
            }
        }
    }

    return(z)
}
