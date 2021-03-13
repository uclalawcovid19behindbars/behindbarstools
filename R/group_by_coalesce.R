comb_vector <- function(x, .method){

    if(all(is.na(x))){
        return(first(x))
    }

    if(.method == "sum" & is.numeric(x)){
        return(sum(x, na.rm = TRUE))
    }

    else if(.method == "sum"){
        return(paste0(unique(as.vector(na.omit(x))), collapse = "; "))
    }

    else{
        return(first(unique(as.vector(na.omit(x)))))
    }

}


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
#' @param .ignore which columns to ignore for warnings
#' @param .method either 'first' or 'sum'
#' @param debug logical, print debug statements
#' @return vector of coalesced values
#'
#' @examples
#' df_safe <- data.frame(
#'     A=c(1,1,2,2,2),
#'     B=c(NA,2,NA,NA,4),
#'     C=c(3,NA,NA,5,NA),
#'     D=c(NA,2,3,NA,NA),
#'     E=c(NA,NA,NA,4,NA))
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
#' @importFrom dplyr enquos
#' @importFrom dplyr group_split
#' @export

group_by_coalesce <- function(.data, ..., .ignore = c(), .method = "first", debug = FALSE) {

    if(!(.method %in% c("first", "sum"))){
        stop(".method should be either 'first' or 'sum'")
    }

    dots <- dplyr::enquos(...)
    z_list <- dplyr::group_split(.data, !!!dots)

    if(!debug){
        out <- .data %>%
            group_by(!!!dots) %>%
            summarize_all(comb_vector, .method = .method) %>%
            ungroup()

        return(out)
    }

    out <- dplyr::bind_rows(lapply(z_list, function(z){

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
                xbar <- as.vector(na.omit(x))
                if(length(unique(xbar)) == 1) {
                    out <- x[[1]]
                }
                else if(length(xbar) != 1 & !(cn %in% .ignore)){
                    warning(paste0(
                        "Group ", z_group_string,
                        " has multiple values that do not match for column ",
                        cn, ". Only using ", .method, " of observed value ",
                        "from the following unique values ",
                        paste0(xbar, collapse = ", ")))
                }
                out <- comb_vector(x, .method = .method)
            }
            out})) %>%
            dplyr::bind_cols(z_group) %>%
            dplyr::select(!!!dots, !!z_cols)

    }))

    return(out)
}
