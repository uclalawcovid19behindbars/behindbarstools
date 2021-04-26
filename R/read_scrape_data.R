#' Read data extracted by webscraper
#'
#' Reads either time series or latest data from the web scraper runs.
#'
#' @param all_dates logical, get all data from all dates recorded by webscraper
#' @param window int, how far to go back (in days) to look for values from a given
#' facility to populate NAs for ALL scraped variables. Used when all_dates is FALSE
#' @param window_pop int, how far to go back (in days) to look for values from a given
#' facility to populate NAs in Residents.Population. Used when coalesce_pop is TRUE
#' @param coalesce_func function, how to combine redundant rows
#' @param drop_noncovid_obs logical, drop rows missing all COVID variables
#' @param debug logical, print debug statements on number of rows maintained in
#' @param state character vector, states to limit data to
#' @param wide_data logical, return wide data as opposed to long
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
    all_dates = FALSE, window = 31, window_pop = 31, coalesce_func = sum_na_rm,
    drop_noncovid_obs = TRUE, debug = FALSE, state = NULL, wide_data = TRUE){

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
        # the following steps are time intensive so its better to do them
        # while the data is wide
        mutate(Name = clean_fac_col_txt(Name, to_upper = TRUE)) %>%
        mutate(
            pop_scraper = ifelse(stringr::str_detect(id, "pop"), T, F),
            historical_covid = ifelse(stringr::str_detect(id, "pre-nov"), T, F)
            ) %>%
        clean_facility_name(debug = debug) %>%
        # if Jurisdiction is NA (no match in facility_spellings),
        # make it scraper jurisdiction
        mutate(Jurisdiction = ifelse(
            (is.na(Jurisdiction) & !is.na(jurisdiction_scraper)),
            jurisdiction_scraper, Jurisdiction)) %>%
        # now we can pivot the data long
        tidyr::pivot_longer(starts_with(c("Residents", "Staff")))

    if(debug){
        message(stringr::str_c(
            "Base data frame contains ", nrow(dat_df), " rows."))
    }

    if(!is.null(state)){
        filt_df <- dat_df %>%
            filter(State %in% state & !is.na(value)) %>%
            as.data.table()

        if(debug){
            message(stringr::str_c(
                "State specific data frame contains ", nrow(filt_df), " rows."))
        }
    }
    else {
        filt_df <- as.data.table(dat_df)[!is.na(value), ]
    }

    # resolve population issues, prioritize dedicated pop scrapers
    pop_full_df <- filt_df[name == "Residents.Population",]
    pop_full_df[,
                singlepop := length(unique(pop_scraper)) == 1,
                by = list(
                    Date, Name, State, jurisdiction_scraper, Facility.ID, name)]
    pop_sub <- pop_full_df[pop_scraper | singlepop,]
    pop_sub[,singlepop := NULL]

    # resolve duplicate scrapers issues, prioritize old scrapers
    cov_full_df <- filt_df[name != "Residents.Population",]
    cov_full_df[,
                singlescrape := length(unique(historical_covid)) == 1,
                by = list(
                    Date, Name, State, jurisdiction_scraper, Facility.ID, name)]
    cov_sub <- cov_full_df[singlescrape | historical_covid,]

    # combine scrapers together
    var_sub_df <- bind_rows(pop_sub,cov_sub) %>%
        select(-pop_scraper, -historical_covid, -singlescrape)

    # Coalesce values together using the passed in coalesce function
    metric_df <- var_sub_df %>%
        select(Date, Name, State, jurisdiction_scraper, Facility.ID, name, value)

    metric_coal_df <- metric_df[,.(value = coalesce_func(value)), by = list(
        Date, Name, State, jurisdiction_scraper, Facility.ID, name
    )]

    # for facilities with missing population data for some days
    # we want to hold valid data for window_pop days
    base_df <- metric_coal_df[
        name == "Residents.Population",
        list(Name, State, jurisdiction_scraper, Facility.ID)] %>%
        unique()

    if(nrow(base_df) > 0){

        fill_dates <- tibble(
            Date = seq.Date(lubridate::ymd("2020-04-01"), Sys.Date(), by = "day")
            )

        full_date_df <- bind_rows(lapply(1:nrow(base_df), function(i){
            bind_cols(fill_dates, base_df[i,])
            })) %>%
            mutate(name = "Residents.Population") %>%
            left_join(
                metric_coal_df[name == "Residents.Population",],
                by = c(
                    "Date", "Name", "State", "jurisdiction_scraper",
                    "Facility.ID", "name")
                ) %>%
            as.data.table() %>%
            mutate(CDate = ifelse(is.na(value), NA_integer_, Date))

        full_date_df[,
            CDate := last_not_na(CDate),
            by = list(Name, State, jurisdiction_scraper, Facility.ID)]

        sub_date_df <- full_date_df[(as.numeric(Date) - CDate) <= window_pop,]
        sub_date_df[,
            value := last_not_na(value),
            by = list(Name, State, jurisdiction_scraper, Facility.ID)
            ]
        sub_date_df[,DDate := Date]
        sub_date_df[,Date := lubridate::as_date(CDate)]
        sub_date_df[,CDate := NULL]

        metric_coal_pop_fix_df <- rbindlist(list(
            metric_coal_df[name != "Residents.Population",] %>%
                mutate(DDate = lubridate::as_date(NA)),
            sub_date_df
            ))
    }
    else{
        metric_coal_pop_fix_df <- metric_coal_df %>%
            mutate(DDate = lubridate::as_date(NA))
    }

    # for ambiguous non metric values such as source use the first value
    non_metric_df <- var_sub_df %>%
        select(-value)

    non_metric_coal_df <- non_metric_df[,lapply(.SD, first), by = list(
        Date, Name, State, jurisdiction_scraper, Facility.ID, name
    )]

    # merge metric and non metric groups together
    comb_df <- left_join(
        metric_coal_pop_fix_df, non_metric_coal_df,
        by = c(
            "Date", "Name", "State", "jurisdiction_scraper",
            "Facility.ID", "name")) %>%
        mutate(Date = lubridate::as_date(ifelse(is.na(DDate), Date, DDate))) %>%
        select(-DDate)

    if(debug){
        message(stringr::str_c(
            "Coalesced data frame contains ", nrow(comb_df), " rows."))
    }

    # merge on facility level info
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
                ungroup() %>%
                group_by(State, Date, Measure, jurisdiction_scraper) %>%
                mutate(has_statewide = "STATEWIDE" %in% Name) %>%
                # if state wide and other counts exist for a measure only take max date
                filter(!(has_statewide) | Date == max(Date)) %>%
                # if state wide and other counts still exist for a measure only
                # use non-statewide
                mutate(has_statewide = "STATEWIDE" %in% Name) %>%
                mutate(has_other = any("STATEWIDE" != Name, na.rm=T)) %>%
                filter(!(has_other & has_statewide & Name == "STATEWIDE")) %>%
                ungroup() %>%
                select(-has_statewide, -has_other)
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
