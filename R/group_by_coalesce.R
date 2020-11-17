#' A all in one group by and coalesce function
#'
#' Groups by and coalesces the remaining columns such that only one value
#' is returned per column. If multiple values are present that are not NA
#' a warning is returned with the offending group, column name, and values.
#'
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy
#' data frame (e.g. from dbplyr or dtplyr).
#' @param ... one or more unquoted expressions separated by commas. Variable
#' names can be used as if they were positions in the data frame,
#' so expressions like x:y can be used to select a range of variable
#' @return vector of coalesced values
#'
#' @examples
#' df_safe <- data.frame(
#'     A=c(1,1,2,2,2),
#'     B=c(NA,2,NA,4,4),
#'     C=c(3,NA,NA,5,NA),
#'     D=c(NA,2,3,NA,NA),
#'     E=c(NA,NA,NA,4,4))
#'
#' df_warn <- data.frame(
#'     A=c(1,1,2,2,2),
#'     B=c(NA,2,NA,4,4),
#'     C=c(3,NA,NA,5,NA),
#'     D=c(NA,2,3,NA,NA),
#'     E=c(NA,NA,NA,4,5))
#'
#' group_by_coalesce(df_safe, A)
#' group_by_coalesce(df_warn, A)
#'
#' @importFrom magrittr %>%
#' @importFrom stats na.omit
#' @import dplyr
#' @export

group_by_coalesce <- function(.data, ...) {

    dots <- dplyr::enquos(...)
    z_list <- dplyr::group_split(.data, !!!dots)

    dplyr::bind_rows(lapply(z_list, function(z){

        z_group <- z %>%
            dplyr::select(!!!dots) %>%
            unique()

        z_sub <- z[,setdiff(names(z), names(z_group))]

        z_cols <- names(z_sub)
        names(z_cols) <- z_cols

        z_group_string <- paste0(sapply(1:length(z_group), function(i){
            paste0(names(z_group)[i], ": ", z_group[[i]])
        }), collapse = ", ")

        dplyr::bind_cols(lapply(z_cols, function(cn){
            x <- z_sub[[cn]]
            if(all(is.na(x))){
                out <- x[[1]]
            }
            else{
                xbar <- unique(as.vector(na.omit(x)))
                if(length(xbar) != 1){
                    warning(paste0(
                        "Group ", z_group_string,
                        " has multiple values that do not match for column ",
                        cn, ". Only using first observed value from the ",
                        "following unique values ",
                        paste0(xbar, collapse = ", ")))
                }
                # only grab the first one
                out <- xbar[1]
            }
            out})) %>%
            dplyr::bind_cols(z_group) %>%
            dplyr::select(!!!dots, !!z_cols)

    }))
}
