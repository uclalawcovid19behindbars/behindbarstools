#' Read in HIFLD Facility level Data
#'
#' Reads in data sheet from HIFLD on facility information including population
#'
#' @return data frame with facility info
#'
#' @importFrom readr read_csv
#' @importFrom readr cols
#' @importFrom utils download.file
#' @export

read_hifld_data <- function(){
    tf <- tempfile(fileext = ".csv")

    "https://opendata.arcgis.com/datasets/" %>%
        paste0(
            "2d6109d4127d458eaf0958e4c5296b67_0.csv?",
            "outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D") %>%
        download.file(tf)

    read_csv(tf, col_types = cols()) %>%
        dplyr::rename(hifld_id = FACILITYID)
}

