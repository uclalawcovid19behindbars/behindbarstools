#' Coalescing join 
#'
#' Wrapper around dplyr::join and dplyr::coalesce to enable coalescing joins 
#' where missing values in `x` are filled in with matching values from `y`. 
#' Syntax borrowed directly from here:
#' https://alistaire.rbind.io/blog/coalescing-joins/. 
#'
#' @param x,y A pair of data frames, data frame extensions (e.g. a tibble), or
#'   lazy data frames (e.g. from dbplyr or dtplyr). 
#' @param by A character vector of variables to join by.
#'
#'   If `NULL`, the default, `*_join()` will perform a natural join, using all
#'   variables in common across `x` and `y`. A message lists the variables so that you
#'   can check they're correct; suppress the message by supplying `by` explicitly.
#'
#'   To join by different variables on `x` and `y`, use a named vector.
#'   For example, `by = c("a" = "b")` will match `x$a` to `y$b`.
#'
#'   To join by multiple variables, use a vector with length > 1.
#'   For example, `by = c("a", "b")` will match `x$a` to `y$a` and `x$b` to
#'   `y$b`. Use a named vector to match different variables in `x` and `y`.
#'   For example, `by = c("a" = "b", "c" = "d")` will match `x$a` to `y$b` and
#'   `x$c` to `y$d`.
#'
#'   To perform a cross-join, generating all combinations of `x` and `y`,
#'   use `by = character()`.
#' @param suffix If there are non-joined duplicate variables in `x` and
#'   `y`, these suffixes will be added to the output to disambiguate them.
#'   Should be a character vector of length 2.
#' @param join Type of join to perform (e.g. dplyr::left_join). 
#' @param debug logical, use coalesce_with_warnings and print debug statements
#' @param ... Other parameters passed onto methods.
#'
#' @return An object of the same type as `x`.
#'
#' @examples
#' df1 <- data.frame(
#'     key  = c('a', 'b', 'c', 'd', 'e', 'f'),
#'     var1 = c(  1,   2,   3,   4,  NA,  NA),
#'     var2 = c( NA,  NA,  NA,  NA,   5,   6),
#'     var3 = c(  1,   2,   3,   4,   5,   6)
#' )
#' 
#' df2 <- data.frame(
#'     key  = c('c', 'd', 'e', 'f'),
#'     var1 = c( NA,  NA,   5,   6),
#'     var2 = c( NA,   4,   5,  NA),
#'     var4 = c(  3,   4,   5,   6)
#' )
#' 
#' coalesce_join(df1, df2, by = 'key')
#'
#' @export

coalesce_join <- function(x, y, 
                          by = NULL, 
                          suffix = c(".x", ".y"), 
                          join = dplyr::full_join, 
                          debug = FALSE, ...) {
    
    joined <- join(x, y, by = by, suffix = suffix, ...)
    # names of desired output
    cols <- union(names(x), names(y))
    
    to_coalesce <- names(joined)[!names(joined) %in% cols]
    suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]
    # remove suffixes and deduplicate
    to_coalesce <- unique(substr(
        to_coalesce, 
        1, 
        nchar(to_coalesce) - nchar(suffix_used)
    ))
    
    if (debug) {
        coalesced <- purrr::map_dfc(
            to_coalesce, ~behindbarstools::coalesce_with_warnings(
            joined[[paste0(.x, suffix[1])]], 
            joined[[paste0(.x, suffix[2])]]
        ))
    } else {
        coalesced <- purrr::map_dfc(
            to_coalesce, ~dplyr::coalesce(
            joined[[paste0(.x, suffix[1])]], 
            joined[[paste0(.x, suffix[2])]]
        ))
    }
    
    names(coalesced) <- to_coalesce
    
    dplyr::bind_cols(joined, coalesced)[cols]
}
