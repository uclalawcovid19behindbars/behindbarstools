#' Simple function that returns TRUE/FALSE if "federal" is detected, case insensitive
#'
#' Simple function that returns TRUE/FALSE if "federal" is detected, case insensitive.
#'
#' @param x string 
#' @return TRUE/FALSE
#'
#' @examples
#' is_federal("FedeRal")
#' is_federal("test")
#' 
#' @import stringr
#'
#' @export
  
is_federal <- function(x) stringr::str_detect(x, "(?i)federal")