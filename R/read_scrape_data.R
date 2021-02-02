#' Read data extracted by webscraper
#'
#' Reads either time series or latest data from the web scraper runs
#'
#' @param all_dates logical, get all data from all dates recorded by webscraper
#' @param coalesce logical, collapse common facilities into single row
#' @param debug logical, print debug statements on number of rows maintained in
#' @param state character vector, states to limit data to
#' cleaning process
#'
#' @return character vector of state names
#'
#' @importFrom assertthat see_if
#'
#' @examples
#' \dontrun{
#' read_scrape_data(all_dates = FALSE)
#' }
#' read_scrape_data(all_dates = TRUE, state = "Wyoming")
#'
#' @export

read_scrape_data <- function(
    all_dates = FALSE, coalesce = TRUE, debug = FALSE, state = NULL){

    remote_loc <- stringr::str_c(
        SRVR_SCRAPE_LOC, "summary_data/aggregated_data.csv")

    jnk <- read.csv(remote_loc, nrows=1, check.names=FALSE)
    # all columns are character columns unless otherwise denoted
    ctypes <- rep("c", ncol(jnk))
    names(ctypes) <- names(jnk)
    # columns that start with residents or staff or data
    ctypes[str_starts(names(ctypes), "Residents|Staff")] <- "d"
    # date is date type
    ctypes[names(ctypes) == "Date"] <- "D"

    if(!all_dates){
        dat_df <- remote_loc %>%
            readr::read_csv(col_types = paste0(ctypes, collapse = "")) %>%
            group_by(State, id, jurisdiction) %>% # `id` here refers to scraper id
            filter(Date == max(Date)) %>%
            ungroup()
    }

    else{
        dat_df <- remote_loc %>%
            readr::read_csv(col_types = paste0(ctypes, collapse = ""))
    }

    if(debug){
        message(stringr::str_c(
            "Base data frame contains ", nrow(dat_df), " rows."))
    }

    dat_df <- dat_df %>%
        mutate(State = translate_state(State)) %>%
        rename(jurisdiction_scraper = jurisdiction) # rename this variable for clarity

    if(!is.null(state)){
        filt_df <- dat_df %>%
            filter(State %in% state)

        if(debug){
            message(stringr::str_c(
                "State specific data frame contains ", nrow(filt_df), " rows."))
        }
    }
    else {
        filt_df <- dat_df
    }

    comb_df <- filt_df %>%
        select(-starts_with("Resident.Deaths")) %>%
        mutate(Name = clean_fac_col_txt(Name, to_upper = TRUE)) %>%
        clean_facility_name(debug = debug) %>%
        # if Jurisdiction is NA (no match in facility_spellings), make it scraper jurisdiction
        mutate(Jurisdiction = ifelse((is.na(Jurisdiction) & !is.na(jurisdiction_scraper)),
                                     jurisdiction_scraper,
                                     Jurisdiction)
               )

    if(coalesce){
        comb_df <- comb_df %>%
            select(-id) %>%
            group_by_coalesce(
                Date, Name, State, jurisdiction_scraper, Facility.ID,
                .ignore = c(
                    "source", "scrape_name_clean", "federal_bool",
                    "xwalk_name_clean", "name_match", "Jurisdiction"),
                .method = "sum", debug = debug)

        if(debug){
            message(stringr::str_c(
                "Coalesced data frame contains ", nrow(comb_df), " rows."))
        }
    }

    out_df <- merge_facility_info(comb_df)

    if(debug){
        message(stringr::str_c(
            "Named data frame contains ", nrow(out_df), " rows."))
    }

    out_df <- out_df %>%
        arrange(State, Name, Date) %>%
        reorder_cols()

    if(!all_dates){
        out_df <- out_df %>%
            group_by(Facility.ID, jurisdiction_scraper, State, Name) %>%
            filter(Date == max(Date)) %>%
            ungroup()
    }

    return(out_df)
}
