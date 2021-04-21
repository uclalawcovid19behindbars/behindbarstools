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
#' @examples
#' \dontrun{
#' read_scrape_data(all_dates = FALSE)
#' }
#' read_scrape_data(all_dates = TRUE, state = "Wyoming")
#'
#' @export

read_scrape_data <- function(
    all_dates = FALSE, window = 31, window_pop = 31, coalesce = TRUE,
    coalesce_pop = TRUE, drop_noncovid_obs = TRUE, debug = FALSE, state = NULL,
    wide_data = TRUE){

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
        readr::read_csv(col_types = paste0(ctypes, collapse = "")) %>%
        mutate(State = translate_state(State)) %>%
        # rename this variable for clarity
        rename(jurisdiction_scraper = jurisdiction) %>%
        select(-starts_with("Resident.Deaths")) %>%
        mutate(Name = clean_fac_col_txt(Name, to_upper = TRUE)) %>%
        mutate(
            pop_scraper = ifelse(stringr::str_detect(id, "pop"), T, F),
            historical_covid = ifelse(stringr::str_detect(id, "pre-nov"), T, F)) %>%
        tidyr::pivot_longer(starts_with(c("Residents", "Staff")))


    if(debug){
        message(stringr::str_c(
            "Base data frame contains ", nrow(dat_df), " rows."))
    }

    cln_name_df <- dat_df %>%
        clean_facility_name(debug = debug) %>%
        # if Jurisdiction is NA (no match in facility_spellings), make it scraper jurisdiction
        mutate(Jurisdiction = ifelse(
            (is.na(Jurisdiction) & !is.na(jurisdiction_scraper)),
            jurisdiction_scraper, Jurisdiction))

    if(!is.null(state)){
        filt_df <- cln_name_df %>%
            filter(State %in% state)

        if(debug){
            message(stringr::str_c(
                "State specific data frame contains ", nrow(filt_df), " rows."))
        }
    }
    else {
        filt_df <- as.data.table(cln_name_df)[!is.na(value), ]
    }

    if(coalesce){

        pop_full_df <- filt_df[name == "Residents.Population",]
        pop_full_df[,
                    singlepop := length(unique(pop_scraper)) == 1,
                    by = list(
                        Date, Name, State, jurisdiction_scraper, Facility.ID, name)]
        pop_sub <- pop_full_df[pop_scraper | singlepop,]
        pop_sub[,singlepop := NULL]

        cov_full_df <- filt_df[name != "Residents.Population",]
        cov_full_df[,
                    singlescrape := length(unique(historical_covid)) == 1,
                    by = list(
                        Date, Name, State, jurisdiction_scraper, Facility.ID, name)]
        cov_sub <- cov_full_df[singlescrape | historical_covid,]

        var_sub_df <- bind_rows(pop_sub,cov_sub) %>%
            select(-pop_scraper, -historical_covid, -singlescrape)

        metric_df <- var_sub_df %>%
            select(Date, Name, State, jurisdiction_scraper, Facility.ID, name, value)

        metric_coal_df <- metric_df[,.(value = coalesce_func(value)), by = list(
            Date, Name, State, jurisdiction_scraper, Facility.ID, name
        )]

        non_metric_df <- var_sub_df %>%
            select(-value)

        non_metric_coal_df <- non_metric_df[,lapply(.SD, first), by = list(
            Date, Name, State, jurisdiction_scraper, Facility.ID, name
        )]

        comb_df <- left_join(
            metric_coal_df, non_metric_coal_df,
            by = c(
                "Date", "Name", "State", "jurisdiction_scraper", "Facility.ID", "name"))

        run_time <- Sys.time() - start_time

        if(debug){
            message(stringr::str_c(
                "Coalesced data frame contains ", nrow(comb_df), " rows."))
        }
    }
    else {
        comb_df <- filt_df
    }

    out_df <- merge_facility_info(comb_df) %>%
        rename(Measure = name) %>%
        select(-id)

    if(!all_dates){
        out_df <- out_df %>%
            # only keep values in the last window of days
            filter(Date >= (Sys.Date() - window)) %>%
            group_by(Facility.ID, jurisdiction_scraper, State, Name, Measure) %>%
            arrange(Facility.ID, jurisdiction_scraper, State, Name, Measure, Date) %>%
            # keep only last observed value
            filter(1:n() == n()) %>%
            # make all the sources the same
            group_by(Facility.ID, jurisdiction_scraper, State, Name) %>%
            mutate(source = first(source)) %>%
            ungroup()
    }

    if(wide_data){
        if(!all_dates){
            out_df <- out_df %>%
                group_by(
                    Facility.ID, jurisdiction_scraper, State, Name) %>%
                mutate(Date = max(Date)) %>%
                ungroup()
        }

        out_df <- out_df %>%
            tidyr::pivot_wider(names_from = Measure, values_from = value)

        if(drop_noncovid_obs){
            rowAny <- function(x) rowSums(x) > 0

            out_df <- out_df %>%
                # drop rows missing COVID data (e.g. only with population data)
                filter(rowAny(across(ends_with(c(
                    ".Confirmed", ".Deaths", ".Recovered", ".Tadmin", ".Tested", ".Active",
                    ".Negative", ".Pending", ".Quarantine", ".Initiated", ".Completed", ".Vadmin")),
                    ~ !is.na(.x))))
        }

        out_df <- out_df %>%
            arrange(Facility.ID, jurisdiction_scraper, State, Name, Date) %>%
            reorder_cols()
    }

    if(debug){
        message(stringr::str_c(
            "Named data frame contains ", nrow(out_df), " rows."))
    }

    return(out_df)
}
