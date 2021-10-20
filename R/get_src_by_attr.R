#' Gets a web element based on a specific attribute that matches regex
#'
#' @param base character string of the URL
#' @param css css option to search html nodes
#' @param xpath xpath option to search html nodes
#' @param attr character the attribute to pull a string from
#' @param attr_regex character regex to search in attributes
#' @param date_regex character string to extract date from attribute
#' @param date_format character the format to convert the date from
#'
#' @return character of the url specified by the search
#'
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes
#' @importFrom rvest html_attr
#' @importFrom tibble tibble
#' @importFrom stringr str_extract
#' @importFrom lubridate parse_date_time
#' @importFrom dplyr mutate
#' @importFrom xml2 url_absolute
#'
#' @examples
#' \dontrun{
#' get_src_by_attr(
#'     base = "https://doc.arkansas.gov/covid-19-updates/",
#'     css = "a",
#'     attr = "href",
#'     attr_regex = "(?i)stats.?update",
#'     date_regex = "\\d+-\\d+-\\d+")
#' }
#'
#' @export

get_src_by_attr <- function(
    base, css, xpath, attr, attr_regex, date_regex = NULL, date_format = "mdy"){

    html_src <- read_html(base)

    web_page_imgs <- html_nodes(html_src, css, xpath)

    srcs <- html_attr(web_page_imgs, attr)

    if(is.null(date_regex)){
        url_portion <- srcs[grepl(attr_regex, srcs)]
    }

    else{
        condition_df <- tibble(
            src_string = srcs,
            match_grep = grepl(attr_regex, srcs),
            date = parse_date_time(
                str_extract(srcs, date_regex), date_format)) %>%
            mutate(val = as.numeric(date)) %>%
            mutate(val = ifelse(match_grep, val, -Inf))

        im_pos <- which(condition_df$val == max(condition_df$val))
        url_portion <- srcs[[im_pos]]
    }

    url_absolute(url_portion, base)
}
