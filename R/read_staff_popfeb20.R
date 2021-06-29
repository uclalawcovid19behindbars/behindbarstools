#' Get Feb 20, 2020 population data for applicable rows in the fac_data
#'
#' Reads the UCLA dataset and for each facility pulls the staff population data
#' closest to Feb 20, 2020 in the UCLA historical data.
#'
#' @return data frame with population data for staff by facility
#'
#' @examples
#' \dontrun{
#' read_staff_popfeb20()
#' }
#' @export

read_staff_popfeb20 <- function(){
    read_scrape_data(
        window = window, all_dates = T, wide_data = FALSE) %>%
        filter(Measure == "Staff.Population") %>%
        filter(!is.na(Facility.ID) & !is.na(value)) %>%
        group_by(Facility.ID) %>%
        mutate(dtime = abs(as.numeric(Date - lubridate::ymd("2020-02-01")))) %>%
        filter(dtime == min(dtime)) %>%
        filter(Date == min(Date)) %>%
        ungroup() %>%
        select(Facility.ID, Name, State, Staff.Population.Feb20 = value)
}
