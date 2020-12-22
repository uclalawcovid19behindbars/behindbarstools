#' Gets the description for a given COVID-19 metric 
#' 
#' Given a COVID-19 metric collected in the scraped data, returns a description of 
#' that metric, primarily useful for plotting (e.g. axis labels, titles, etc.). 
#'
#' @param metric character string of the given metric 
#' @param short logical, whether to return the short (vs. long) description 
#' 
#' @return character string of the given metric's description 
#' 
#' @examples
#' get_metric_description("Residents.Active")
#' 
#' @importFrom stringr str_c
#'
#' @export

get_metric_description <- function(metric, short = FALSE) {
    short_lookup <- c(
        "Residents.Active" = "Active COVID-19 Cases", 
        "Staff.Active" = "Active COVID-19 Cases", 
        "Residents.Confirmed" = "Cumulative COVID-19 Cases", 
        "Staff.Confirmed" = "Cumulative COVID-19 Cases", 
        "Residents.Deaths" = "Cumulative COVID-19 Deaths", 
        "Staff.Deaths" = "Cumulative COVID-19 Deaths", 
        "Residents.Tadmin" = "COVID-19 Tests" 
    )
    
    long_lookup <- c(
        "Residents.Active" = "Current Number of Active COVID-19 Cases Among Incarcerated Residents", 
        "Staff.Active" = "Current Number of Active COVID-19 Cases Among Staff", 
        "Residents.Confirmed" = "Cumulative Number of Confirmed COVID-19 Cases Among Incarcerated Residents", 
        "Staff.Confirmed" = "Cumulative Number of Confirmed COVID-19 Cases Among Staff", 
        "Residents.Deaths" = "Cumulative Number of Deaths Due to COVID-19 Among Incarcerated Residents", 
        "Staff.Deaths" = "Cumulative Number of Deaths Due to COVID-19 Among Staff", 
        "Residents.Tadmin" = "Cumulative Number of COVID-19 Tests Administered"
    )
    
    if (short) {
        if (metric %in% names(short_lookup)) {
            rv <- short_lookup[metric] %>% 
                unname()
        } else {
            stop(str_c(metric, " is not a valid metric name.\n  Metrics include: ", 
                        toString(names(short_lookup))))}
    } else {
        if (metric %in% names(long_lookup)) {
            rv <- long_lookup[metric] %>% 
                unname()
        } else {
            stop(str_c(metric, " is not a valid metric name.\n  Metrics include: ", 
                       toString(names(long_lookup))))}
    }
    
    return(rv)
}
