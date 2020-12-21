#' Plot a single facility metric 
#'
#' Creates time series plots showing the trend for a single facility for a given
#' metric, formatted according to the style guide. 
#'
#' @param fac_name character string of the facility to plot 
#' @param state character string of the facility's state 
#' @param metric character string of the metric to plot 
#' @param scrape_df data frame with scraped data 
#' @param plot_days integer, number of days to plot  
#' @param annotate logical, whether to include text annotations for most recent value  
#' 
#' @return ggplot object 
#' 
#' @examples
#' plot_fac_trend("Los Angeles Jails", "California", "Residents.Active")
#' 
#' @import ggplot2
#' @import dplyr 
#' @importFrom ggrepel geom_label_repel
#' @importFrom stringr str_c
#' @importFrom stringr str_to_upper
#'
#' @export

plot_fac_trend <- function(
    fac_name, state, metric, scrape_df = NULL, plot_days = NULL, annotate = F) {
    
    if (! behindbarstools::is_fac_name(fac_name, state)) {
        stop(str_c(fac_name, " in ", state, " is not a valid facility."))
    }
    
    if (is.null(scrape_df)) {
        scrape_df <- read_scrape_data(T, T, state = state)
    }
    
    plot_end_date <-  max(scrape_df$Date)
    if (is.null(plot_days)) {
        plot_start_date <- min(scrape_df$Date)
    }
    else {
        plot_start_date <- plot_end_date - lubridate::days(plot_days)
    }
    
    scrape_df %>% 
        filter(!is.na(!!sym(metric))) %>% 
        filter(Date >= plot_start_date) %>% 
        filter(State == state) %>% 
        filter(Name == behindbarstools::clean_fac_col_txt(fac_name, T)) %>%  
        mutate(last_value = if_else(Date == max(Date), as.character(!!sym(metric)), NA_character_)) %>% 
        ggplot(aes(x = Date, y = !!sym(metric), color = fac_name, label = last_value)) +
        geom_line(size = 2.0) +
        geom_point(size = 3.0) +
        {if (annotate) 
            geom_label_repel(na.rm = T, show.legend = F, label.r = 0, size = 6)} + 
        labs(title = str_c(get_metric_description(metric, short = T), " in ", fac_name),
             subtitle = get_metric_description(metric), 
             x = "Date",
             y = get_metric_description(metric, short = T), 
             caption = str_to_upper("UCLA Law COVID-19 Behind Bars Data Project")) +
        theme_behindbars() +
        scale_color_bbdiscrete() +
        theme(legend.position = "none", 
              axis.title.x = element_blank())
}
