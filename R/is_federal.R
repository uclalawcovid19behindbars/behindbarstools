#' Simple function that returns TRUE/FALSE if "federal" is detected, case insensitive
#'
#' Simple function that returns TRUE/FALSE if "federal" is detected, case insensitive.
#'
#' @param x string
#' @param ... inherited argument from str_detect
#' @return TRUE/FALSE
#'
#' @examples
#' is_federal("FedeRal")
#' is_federal("test")
#'
#' @importFrom stringr str_detect
#'
#' @export

is_federal <- function(x, ...) str_detect(x, "(?i)federal", ...)
