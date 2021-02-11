#' Re-orders columns to respect structure of daily scraped data.
#'
#' Reorders columns in a data frame to agree with the existing structure.
#' Throws a warning for missing columns and optionally adds them in as all NA rows.
#' Throws a warning for extra columns and optionally keeps them at the end of the dataframe.
#'
#' @param data A data frame, data frame extension (e.g. a tibble), or a lazy
#' data frame (e.g. from dbplyr or dtplyr).
#' @param add_missing_cols logical, add missing columns as all NA rows
#' @param rm_extra_cols logical, remove extra columns
#'
#' @return re-ordered data frame
#'
#' @examples
#' test_data <- tibble::tibble("jurisdiction" = "jail",
#'                     "City" = "Los Angeles")
#' reorder_cols(test_data)
#'
#' @importFrom dplyr relocate
#' @importFrom dplyr any_of
#'
#' @export

reorder_cols <- function(data, add_missing_cols=TRUE, rm_extra_cols=FALSE) {
    scraper_cols <- c(
        "Facility.ID", "Jurisdiction", "State", "Name", "Date", "source",
        "Residents.Confirmed", "Staff.Confirmed", "Residents.Deaths",
        "Staff.Deaths", "Residents.Recovered","Staff.Recovered",
        "Residents.Tadmin", "Staff.Tested", "Residents.Negative",
        "Staff.Negative", "Residents.Pending", "Staff.Pending",
        "Residents.Quarantine", "Staff.Quarantine", "Residents.Active",
        "Population.Feb20", "Residents.Population", "Residents.Tested",
        "Residents.Initiated", "Residents.Completed", "Residents.Vadmin",
        "Staff.Initiated", "Staff.Completed", "Staff.Vadmin",
        "Address", "Zipcode", "City", "County", "Latitude", "Longitude",
        "County.FIPS", "HIFLD.ID", "jurisdiction_scraper", "Description",
        "Security", "Age", "Gender", "Is.Different.Operator",
        "Different.Operator", "Capacity", "BJS.ID", "Source.Population.Feb20",
        "Source.Capacity", "Website", "ICE.Field.Office")
    these_cols <- names(data)
    missing_cols <- if(all(scraper_cols %in% these_cols)) { NULL } else(base::setdiff(scraper_cols, these_cols))
    additional_cols <- if(all(these_cols %in% scraper_cols)) { NULL } else(base::setdiff(these_cols, scraper_cols))

    # re-order any columns that exist to start with
    data <- data %>%
        relocate(any_of(scraper_cols))

    if(rm_extra_cols & (length(additional_cols) > 0 )){
         add_out = data %>%
            select(-all_of(additional_cols))
    }
    else{
        if(length(additional_cols) > 0) {
            warning(paste0("Input data has ", length(additional_cols),
            " additional columns: ", paste0(additional_cols, collapse = ", "),
            ". Moving these to the end of the data set."))
            add_out <- data %>%
                relocate(additional_cols, .after = last_col())
        }
        if(length(additional_cols) == 0) {
            add_out = data
        }
    }
    if(!add_missing_cols & (length(missing_cols) > 1 )) {
        warning(paste0("Input data has ", length(missing_cols),
        " missing columns: ", paste0(missing_cols, collapse = ", "),
        ". This could cause problems when binding rows down the line."))
        missing_out <- add_out
    }
    else{
        if(length(missing_cols) > 0) {
            warning(paste0("Input data has ", length(missing_cols),
            " missing columns: ", paste0(missing_cols, collapse = ", "),
            ". Adding these columns in as NA rows."))
            add_out[,missing_cols] <- NA
            missing_out <- add_out
        }
        if(length(missing_cols) == 0) {
            missing_out <- add_out
        }
    }
    out <- missing_out %>%
        relocate(any_of(scraper_cols))
    return(out)
}
