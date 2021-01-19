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

    remote_loc <- stringr::str_c(SRVR_SCRAPE_LOC, "summary_data/")

    if(!all_dates){
        dat_df <- remote_loc %>%
            stringr::str_c("aggregated_data.csv") %>%
            readr::read_csv(col_types = "Dccccddddddddddcddddddd") %>%
            group_by(State, id, jurisdiction) %>% # `id` here refers to scraper id
            filter(Date == max(Date)) %>%
            ungroup()
    }

    else{
        dat_df <- remote_loc %>%
            stringr::str_c("aggregated_data.csv") %>%
            readr::read_csv(col_types = "Dccccddddddddddcddddddd")
    }

    if(debug){
        message(stringr::str_c(
            "Base data frame contains ", nrow(dat_df), " rows."))
    }

    comb_df <- dat_df %>%
        select(-starts_with("Resident.Deaths")) %>%
        mutate(Name = clean_fac_col_txt(Name, to_upper = TRUE)) %>%
        mutate(State = translate_state(State)) %>%
        clean_facility_name(debug = debug)

    if(coalesce){
        comb_df <- comb_df %>%
            select(-id) %>%
            group_by_coalesce(
                Date, Name, State, jurisdiction, Facility.ID,
                .ignore = c(
                    "source", "scrape_name_clean", "federal_bool",
                    "xwalk_name_clean", "name_match"),
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

    if(!is.null(state)){
        out_df <- out_df %>%
            filter(State %in% state)

        if(debug){
            message(stringr::str_c(
                "State specific data frame contains ", nrow(out_df), " rows."))
        }
    }

    if(debug){
        # leave all columns present for debugging
        out_df <- out_df %>%
            arrange(State, Name, Date)
    }
    else {
        out_df <- out_df %>%
            # Select the order for names corresponding to Public facing Google sheet
            select(
                Facility.ID, Jurisdiction, State, Name, Date, source,
                Residents.Confirmed, Staff.Confirmed,
                Residents.Deaths, Staff.Deaths, Residents.Recovered,
                Staff.Recovered, Residents.Tadmin, Staff.Tested, Residents.Negative,
                Staff.Negative, Residents.Pending, Staff.Pending,
                Residents.Quarantine, Staff.Quarantine, Residents.Active,
                Residents.Popultion, Residents.Tested,
                Population.Feb20, Address, Zipcode, City, County, Latitude,
                Longitude, County.FIPS, HIFLD.ID) %>%
            arrange(State, Name, Date)
    }
    return(out_df)
}
