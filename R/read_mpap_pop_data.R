#' Get Feb 20, 2020 population data for state prisons from the MP
#'
#' Reads the MP dataset and for each prison jurisdiction pulls the staff and
#' resident population data closest to Feb 20, 2020.
#'
#' @return data frame with jurisdiction level pop data from MP
#'
#' @examples
#' \dontrun{
#' read_staff_popfeb20()
#' }
#' @export


read_mpap_pop_data <- function(){
    pri_pop_df <- "https://raw.githubusercontent.com/themarshallproject/" %>%
        str_c("COVID_prison_data/master/data/prison_populations.csv") %>%
        readr::read_csv(col_types = readr::cols()) %>%
        mutate(Date = lubridate::mdy(as_of_date)) %>%
        mutate(dtime = abs(as.numeric(Date - lubridate::ymd("2020-02-01")))) %>%
        group_by(name) %>%
        filter(dtime == min(dtime)) %>%
        filter(Date == min(Date)) %>%
        ungroup() %>%
        select(State = name, Population.Feb20 = pop)

    staff_pop_df <- "https://raw.githubusercontent.com/themarshallproject/" %>%
        str_c("COVID_prison_data/master/data/staff_populations.csv") %>%
        readr::read_csv(col_types = readr::cols()) %>%
        mutate(Date = lubridate::mdy(as_of_date)) %>%
        mutate(dtime = abs(as.numeric(Date - lubridate::ymd("2020-02-01")))) %>%
        group_by(name) %>%
        filter(dtime == min(dtime)) %>%
        filter(Date == min(Date)) %>%
        ungroup() %>%
        select(State = name, Staff.Population.Feb20 = pop)

    full_join(pri_pop_df, staff_pop_df, by = "State")
}
