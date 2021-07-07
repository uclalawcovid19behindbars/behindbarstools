#' Given a URL returns the url, timestamp, and statuscodes for wayback machine
#'
#' given a URL returns the time stamped url, timestamp, and statuscodes
#' for wayback machine archived versions of the website.
#'
#' @param url character, url to check wayback archives for
#' @return data frame like object with columns for original url, timestamp,
#' statuscode and url
#'
#' @examples
#' \dontrun{
#' wayback_archives("https://doccs.ny.gov/doccs-covid-19-report")
#' }
#'
#' @export

wayback_archives <- function(url) {

    if (length(url) > 1) {
        warning("More than one URL provided. Only using the first URL")
        url <- url[1]
    }

    api_url <- stringr::str_c("http://web.archive.org/cdx/search/cdx?url=", url)

    html_response <- xml2::read_html(api_url)

    response_mat <- html_response %>%
        rvest::html_text() %>%
        stringr::str_split("\n") %>%
        unlist() %>%
        # last row is just a new line thats empty
        .[.!=""] %>%
        # this api should always return 7 rows
        stringr::str_split_fixed(" ",7)

    colnames(response_mat) <- c(
        "urlkey", "timestamp", "original", "mimetype",
        "statuscode", "digest", "length")

    as_tibble(response_mat) %>%
        mutate(url = stringr::str_c(
            "https://web.archive.org/web/", timestamp, "/", original)) %>%
        mutate(timestamp = lubridate::ymd_hms(timestamp)) %>%
        select(original, timestamp, statuscode, url)
}


