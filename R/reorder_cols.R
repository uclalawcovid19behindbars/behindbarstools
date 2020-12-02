#' Re-orders columns to respect structure of daily scraped data. 
#'
#' Reorders columns in a data frame to agree with the existing structure.
#' Throws a warning for missing columns and adds them in as all NA rows.
#' Throws a warning for extra columns and puts them on the end.
#' 
#' @param data A data frame, data frame extension (e.g. a tibble), or a lazy
#' data frame (e.g. from dbplyr or dtplyr).
#' @param rm_extra_cols logical, remove extra columns 
#'
#' @return cleaned character vector
#'
#' @examples
#' clean_fac_col_txt(" Messy string \n\r data   ")
#'
#' @import magrittr
#' @import stringr
#' @export
#' 
data <- dat
test_these <- names(out)

reorder_cols <- function(data, add_missing_cols=TRUE, rm_extra_cols=FALSE) {
    scraper_cols <- c("ID", "jurisdiction", "State", "Name", "Date", "source", "Residents.Confirmed", 
        "Staff.Confirmed", "Residents.Deaths", "Staff.Deaths", "Residents.Recovered", 
        "Staff.Recovered", "Residents.Tadmin", "Staff.Tested", "Residents.Negative", 
        "Staff.Negative", "Residents.Pending", "Staff.Pending", "Residents.Quarantine", 
        "Staff.Quarantine", "Residents.Active", "Residents.Population", "Address", 
        "Zipcode", "City", "County", "Latitude", "Longitude", "County.FIPS", 
        "hifld_id", "Notes")
    these_cols <- names(data)
    missing_cols <- if(all(scraper_cols %in% these_cols)) { NULL } else(setdiff(scraper_cols, these_cols))
    additional_cols <- if(all(these_cols %in% scraper_cols)) { NULL } else(setdiff(these_cols, scraper_cols))

    # re-order any columns that exist to start with
    data <- data %>% 
        relocate(any_of(scraper_cols))

    if(rm_extra_cols & (length(additional_cols) > 1 )){
        add_out = data[,!(these_cols %in% additional_cols)]
    }
    else{
        if(length(additional_cols) > 1) {
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
        if(length(missing_cols) > 1) {
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

try_1 <- reorder_cols(dat)