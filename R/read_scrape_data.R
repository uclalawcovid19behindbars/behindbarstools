#' Read data extracted by webscraper
#'
#' Reads either time series or latest data from the web scraper runs
#'
#' @param all_dates logical, get all data from all dates recorded by webscaraper
#' @param coalesce logical, collapse common facilities into single row
#' @param debug logical, print debug statements on number of rows maintained in
#' cleaning process
#'
#' @return character vector of state names
#'
#' @examples
#' read_scrape_data(all_dates = FALSE)
#'
#' @export

read_scrape_data <- function(all_dates = FALSE, coalesce = TRUE, debug = FALSE){

    facd_df <- "https://raw.githubusercontent.com/uclalawcovid19behindbars" %>%
        stringr::str_c("/facility_data/master/data_sheets/fac_data.csv") %>%
        readr::read_csv(col_types = readr::cols()) %>%
        select(
            ID = Count.ID, State, Name, Address, Zipcode, City, County,
            Latitude, Longitude, County.FIPS, hifld_id) %>%
        mutate(Name = clean_fac_col_txt(stringr::str_to_upper(Name))) %>%
        unique()

    facdf_df <- "https://raw.githubusercontent.com/uclalawcovid19behindbars" %>%
        stringr::str_c("/facility_data/master/data_sheets/fac_data.csv") %>%
        readr::read_csv(col_types = readr::cols()) %>%
        filter(stringr::str_detect(Jurisdiction, "(?i)federal")) %>%
        select(
            ID = Count.ID, State, Name, Address, Zipcode, City, County,
            Latitude, Longitude, County.FIPS, hifld_id,
            hifld_pop = POPULATION) %>%
        mutate(Name = clean_fac_col_txt(stringr::str_to_upper(Name))) %>%
        unique()

    facn_df <- "https://raw.githubusercontent.com/uclalawcovid19behindbars" %>%
        stringr::str_c(
            "/facility_data/master/data_sheets/fac_spellings.csv") %>%
        readr::read_csv(col_types = readr::cols()) %>%
        select(
            ID = Count.ID, State, Name = facility_name_clean,
            Facility = facility_name_raw) %>%
        mutate(Name = clean_fac_col_txt(stringr::str_to_upper(Name))) %>%
        mutate(Facility = clean_fac_col_txt(
            stringr::str_to_upper(Facility))) %>%
        unique()

    if(!all_dates){
        dat_df <- "http://nmmarquez.twilightparadox.com:3838/summary_data/" %>%
            stringr::str_c("aggregated_data.csv") %>%
            readr::read_csv(col_types = "Dccccddddddddddcddddddd") %>%
            group_by(State, id, jurisdiction) %>%
            filter(Date == max(Date)) %>%
            ungroup()
    }

    else{
        dat_df <- "http://nmmarquez.twilightparadox.com:3838/summary_data/" %>%
            stringr::str_c("aggregated_data.csv") %>%
            readr::read_csv(col_types = "Dccccddddddddddcddddddd")
    }

    raw_df <- dat_df %>%
        select(-starts_with("Resident.Deaths")) %>%
        rename(Facility = Name) %>%
        mutate(Facility = clean_fac_col_txt(
            stringr::str_to_upper(Facility))) %>%
        mutate(State = translate_state(State))

    if(debug){
        message(stringr::str_c(
            "Base data frame contains ", nrow(raw_df), " rows."))
    }

    nonfederal_unname <- raw_df %>%
        filter(State != "Federal") %>%
        left_join(facn_df, by = c("Facility", "State"))

    nonfederal <- nonfederal_unname %>%
        mutate(Name = ifelse(is.na(Name), Facility, Name)) %>%
        select(-Facility) %>%
        left_join(facd_df,  by = c("Name", "State", "ID"))

    federal_unname <- facn_df %>%
        filter(State == "Federal") %>%
        group_by(Facility) %>%
        mutate(tmp = 1:n()) %>%
        filter(tmp == 1) %>%
        select(-tmp) %>%
        ungroup() %>%
        right_join(
            raw_df %>%
                filter(jurisdiction == "federal") %>%
                select(-State) %>%
                group_by(Date, Facility, jurisdiction, id, source) %>%
                summarize_all(sum_na_rm) %>%
                ungroup(),
            by = "Facility")

    federal <- federal_unname %>%
        mutate(Name = ifelse(is.na(Name), Facility, Name)) %>%
        select(-Facility, -State) %>%
        left_join(facdf_df,  by = c("Name", "ID")) %>%
        mutate(State = ifelse(is.na(State), "Not Available", State))

    full_df <- bind_rows(federal, nonfederal)

    if(debug){
        message(stringr::str_c(
            "Named data frame contains ", nrow(full_df), " rows."))
    }

    out_df <- full_df %>%
        mutate(Residents.Released = NA, Notes = NA)

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

    pop_df %>%
        mutate(Residents.Population = Population) %>%
        # fill in HIFLD pop where no alternative exists
        mutate(Residents.Population = ifelse(
            is.na(Residents.Population), hifld_pop, Residents.Population)) %>%
        # Select the order for names corresponding to Public facing google sheet
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
