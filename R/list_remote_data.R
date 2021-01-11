#' List remote data locations for Behind Bars Data
#'
#' List remote data locations for Behind Bars Data for either raw_files,
#' log_files, or extracted_data. a scraper_name may also be provided to pull
#' only a specific subset of locations.
#'
#' @param folder character, a single character indicating the folder to pull
#' file locations from. Must be one of "raw_files", "log_files", or
#' "extracted_data"
#' @param scraper_name character, a character string indicating a particular
#' scraper to pull data for. Defaults to NULL which pulls all file locations
#'
#' @return character vector of file location urls
#'
#' @examples
#' list_remote_data("log_files")
#' list_remote_data("raw_files", "lasd")
#'
#' @export

list_remote_data <- function(folder, scraper_name = NULL){

    folders <- c("raw_files", "log_files", "extracted_data")

    if(!(folder %in% folders) | (length(folder) != 1)){
        stop(
            "Folder must be length one and  one of ",
            paste0(folders, collapse = ", "))
    }

    raw <- xml2::read_html(paste0(SRVR_SCRAPE_LOC, folder, "/"))

    out_files <- raw %>%
        rvest::html_nodes(".code") %>%
        rvest::html_attr("href") %>%
        .[!stringr::str_ends(., "/")] %>%
        {paste0(paste0(SRVR_SCRAPE_LOC, folder, "/"), .)}

    if(!is.null(scraper_name)){
        srch_str <- paste0("\\d+-\\d+-\\d+_", scraper_name, "\\..*")
        out_files <- out_files[stringr::str_ends(out_files, srch_str)]
    }

    return(out_files)
}