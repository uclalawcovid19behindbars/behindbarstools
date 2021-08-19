#' Read the latest COVID Prison Project State Data
#'
#' Reads the COVID Prison Project dataset and converts variable names to be 
#' comparable to the UCLA dataset
#'
#' @return data frame with COVID Prison Project results
#'
#' @examples
#' read_cpp_data()
#' 
#' @export

read_cpp_data <- function(){
    cpp_raw_df <- "https://covidprisonproject.com/data/national-overview/" %>% 
        rvest::read_html() %>% 
        rvest::html_nodes('table') %>% 
        .[[2]] %>% 
        rvest::html_table()
    
    rename_df <- cpp_raw_df %>% 
        mutate(
            State = case_when(`Prison System` == "Federal BOP" ~ "Federal", 
                              `Prison System` == "ICE" ~ "ICE", 
                              TRUE ~  translate_state(`Prison System`)), 
            Date = lubridate::mdy(`Scrape Date`)) %>% 
        select(
            State, 
            Date, 
            Residents.Confirmed = `Incarcerated Positive`, 
            Residents.Deaths = `Incarcerated Deaths`, 
            Staff.Confirmed = `Staff Positive`, 
            Staff.Deaths = `Staff Deaths`, 
            Residents.Tadmin = `Incarcerated Tested`) %>% 
        mutate(
            across(starts_with(c("Residents.", "Staff.")), ~ na_if(., "NR")), 
            across(starts_with(c("Residents.", "Staff.")), ~ as.numeric(gsub(",", "", .)))) %>% 
        filter(!is.na(State)) 
    
    return(rename_df)
}
