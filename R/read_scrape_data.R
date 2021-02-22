#' Read data extracted by webscraper
#'
#' Reads either time series or latest data from the web scraper runs.
#'
#' @param all_dates logical, get all data from all dates recorded by webscraper
#' @param window int, how far to go back (in days) to look for values from a given
#' facility to populate NAs for ALL scraped variables. Used when all_dates is FALSE
#' @param window_pop int, how far to go back (in days) to look for values from a given
#' facility to populate NAs in Residents.Population. Used when coalesce_pop is TRUE
#' @param coalesce logical, collapse common facilities into single row
#' @param coalesce_pop logical, collapse population data based on the date window
#' @param drop_noncovid_obs logical, drop rows missing all COVID variables
#' @param debug logical, print debug statements on number of rows maintained in
#' @param state character vector, states to limit data to
#'
#' @return dataframe with scraped data
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
    all_dates = FALSE, window = 31, window_pop = 31, coalesce = TRUE,
    coalesce_pop = TRUE, drop_noncovid_obs = TRUE, debug = FALSE, state = NULL){

    remote_loc <- stringr::str_c(
        SRVR_SCRAPE_LOC, "summary_data/aggregated_data.csv")

    jnk <- read.csv(remote_loc, nrows=1, check.names=FALSE)
    # all columns are character columns unless otherwise denoted
    ctypes <- rep("c", ncol(jnk))
    names(ctypes) <- names(jnk)
    # columns that start with residents or staff or data
    ctypes[stringr::str_starts(names(ctypes), "Residents|Staff")] <- "d"
    # date is date type
    ctypes[names(ctypes) == "Date"] <- "D"

    dat_df <- remote_loc %>%
        readr::read_csv(col_types = paste0(ctypes, collapse = ""))

    if(debug){
        message(stringr::str_c(
            "Base data frame contains ", nrow(dat_df), " rows."))
    }

    dat_df <- dat_df %>%
        mutate(State = translate_state(State)) %>%
        rename(jurisdiction_scraper = jurisdiction) # rename this variable for clarity

    cln_name_df <- dat_df %>%
        select(-starts_with("Resident.Deaths")) %>%
        mutate(Name = clean_fac_col_txt(Name, to_upper = TRUE)) %>%
        clean_facility_name(debug = debug) %>%
        # if Jurisdiction is NA (no match in facility_spellings), make it scraper jurisdiction
        mutate(Jurisdiction = ifelse((is.na(Jurisdiction) & !is.na(jurisdiction_scraper)),
                                     jurisdiction_scraper,
                                     Jurisdiction)
        )

    if(!is.null(state)){
        filt_df <- cln_name_df %>%
            filter(State %in% state)

        if(debug){
            message(stringr::str_c(
                "State specific data frame contains ", nrow(filt_df), " rows."))
        }
    }
    else {
        filt_df <- cln_name_df
    }

    if(coalesce){
        comb_df <- filt_df %>%
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
    else {
        comb_df <- filt_df
    }

    out_df <- merge_facility_info(comb_df)

    if(coalesce_pop){
        out_df <- out_df %>%
            arrange(Facility.ID, Date) %>%
            group_by(Facility.ID) %>%
            # replace NA Residents.Population with values within date window
            mutate(pop_date_ = ifelse(is.na(Residents.Population), NA, Date),
                   pop_date_ = last_not_na(pop_date_),
                   pop_fill_ = last_not_na(Residents.Population),
                   Residents.Population = ifelse(
                       !is.na(Facility.ID) & Date - pop_date_ < window_pop,
                       pop_fill_, Residents.Population)) %>%
            select(-ends_with("_"))
    }

    if(drop_noncovid_obs){
        rowAny <- function(x) rowSums(x) > 0

        out_df <- out_df %>%
            # drop rows missing COVID data (e.g. only with population data)
            filter(rowAny(across(ends_with(c(
                ".Confirmed", ".Deaths", ".Recovered", ".Tadmin", ".Tested", ".Active",
                ".Negative", ".Pending", ".Quarantine", ".Initiated", ".Completed", ".Vadmin")),
                ~ !is.na(.x))))
    }

    if(debug){
        message(stringr::str_c(
            "Named data frame contains ", nrow(out_df), " rows."))
    }

    out_df <- out_df %>%
        arrange(State, Name, Date) %>%
        reorder_cols()

    if(!all_dates){
        out_df <- out_df %>%
            # only keep values in the last window of days
            filter(Date >= (Sys.Date() - window)) %>%
            group_by(Facility.ID, jurisdiction_scraper, State, Name) %>%
            arrange(Facility.ID, jurisdiction_scraper, State, Name, Date) %>%
            # replace all missing values with last observed real value
            mutate(across(starts_with("Residents"), last_not_na)) %>%
            mutate(across(starts_with("Staff"), last_not_na)) %>%
            filter(Date == max(Date)) %>%
            ungroup()
    }

    return(out_df)
}
