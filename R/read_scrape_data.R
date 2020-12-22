#' Read data extracted by webscraper
#'
#' Reads either time series or latest data from the web scraper runs
#'
#' @param all_dates logical, get all data from all dates recorded by webscaraper
#' @param coalesce logical, collapse common facilities into single row
#' @param debug logical, print debug statements on number of rows maintained in
#' @param state character vector, states to limit data to
#' cleaning process
#'
#' @return character vector of state names
#'
#' @examples
#' read_scrape_data(all_dates = FALSE)
#' read_scrape_data(all_dates = TRUE, state = "Wyoming")
#'
#' @export

read_scrape_data <- function(
    all_dates = FALSE, coalesce = TRUE, debug = FALSE, state = NULL){

    if(!all_dates){
        dat_df <- "http://104.131.72.50:3838/scraper_data/summary_data/" %>%
            stringr::str_c("aggregated_data.csv") %>%
            readr::read_csv(col_types = "Dccccddddddddddcddddddd") %>%
            group_by(State, id, jurisdiction) %>%
            filter(Date == max(Date)) %>%
            ungroup()
    }

    else{
        dat_df <- "http://104.131.72.50:3838/scraper_data/summary_data/" %>%
            stringr::str_c("aggregated_data.csv") %>%
            readr::read_csv(col_types = "Dccccddddddddddcddddddd")
    }

    if(debug){
        message(stringr::str_c(
            "Base data frame contains ", nrow(dat_df), " rows."))
    }

    if(debug){
        full_df <- dat_df %>%
            select(-starts_with("Resident.Deaths")) %>%
            mutate(Name = clean_fac_col_txt(Name, to_upper = TRUE)) %>%
            mutate(State = translate_state(State)) %>%
            clean_facility_name(debug = TRUE) %>%
            left_join(read_fac_info(), by = c("Name", "State")) %>%
            rename(HIFLD.Population = POPULATION)
    }
    else{
        full_df <- dat_df %>%
            select(-starts_with("Resident.Deaths")) %>%
            mutate(Name = clean_fac_col_txt(Name, to_upper = TRUE)) %>%
            mutate(State = translate_state(State)) %>%
            clean_facility_name() %>%
            left_join(read_fac_info(), by = c("Name", "State")) %>%
            rename(HIFLD.Population = POPULATION)
    }

    if(debug){
        message(stringr::str_c(
            "Named data frame contains ", nrow(full_df), " rows."))
    }

    out_df <- full_df %>%
        mutate(Residents.Released = NA, Notes = NA)

    if(!is.null(state)){
        out_df <- out_df %>%
            filter(State %in% state)

        if(debug){
            message(stringr::str_c(
                "State specific data frame contains ", nrow(out_df), " rows."))
        }
    }

    if(coalesce){
        out_df <- out_df %>%
            select(-id) %>%
            group_by_coalesce(
                Date, Name, State, jurisdiction,
                .ignore = "source", .method = "sum")

        if(debug){
            message(stringr::str_c(
                "Coalesced data frame contains ", nrow(out_df), " rows."))
        }
    }

    pop_df <- out_df  %>%
        left_join(
            read_pop_data(),
            by = c("Name", "State")
        )

    if(debug){
        message(stringr::str_c(
            "Pop data frame contains ", nrow(pop_df), " rows."))
    }

    pop_df <- pop_df %>%
        mutate(Residents.Population = Population) %>%
        # fill in HIFLD pop where no alternative exists
        mutate(Residents.Population = ifelse(
            is.na(Residents.Population), HIFLD.Population, Residents.Population))

    if(debug){
        # leave all columns present for debugging
        pop_df <- pop_df %>%
            arrange(State, Name, Date)
    }
    else {
        pop_df <- pop_df %>%
            # Select the order for names corresponding to Public facing Google sheet
            select(
                ID, jurisdiction, State, Name, Date, source,
                Residents.Confirmed, Staff.Confirmed,
                Residents.Deaths, Staff.Deaths, Residents.Recovered,
                Staff.Recovered, Residents.Tadmin, Staff.Tested, Residents.Negative,
                Staff.Negative, Residents.Pending, Staff.Pending,
                Residents.Quarantine, Staff.Quarantine, Residents.Active,
                Residents.Population, Address, Zipcode, City, County, Latitude,
                Longitude, County.FIPS, hifld_id, Notes) %>%
            arrange(State, Name, Date)
    }
    return(pop_df)
}
