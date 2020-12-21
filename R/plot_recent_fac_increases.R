#' Plot facilities with recent COVID-19 metric increases  
#'
#' Creates time series plots showing trends for facilities with the steepest recent 
#' increases in a given metric, formatted according to the style guide. 
#'
#' @param scrape_df data frame with scraped data 
#' @param metric character string of the metric to plot 
#' @param delta_days integer, number of days to calculate increase over  
#' @param plot_days integer, number of days to plot  
#' @param num_fac integer, number of facilities to plot 
#' @param annotate logical, whether to include text annotations for most recent value 
#' 
#' @return ggplot object 
#' 
#' @examples
#' \dontrun{
#' plot_recent_fac_increases()
#' plot_recent_fac_increases(metric = "Residents.Deaths", plot_days = 30, num_fac = 3)
#' }
#' 
#' @import ggplot2
#' @import dplyr 
#' @importFrom ggrepel geom_label_repel
#' @importFrom stringr str_c
#' @importFrom stringr str_detect
#' @importFrom stringr str_to_title
#' @importFrom stringr str_to_upper
#'
#' @export

plot_recent_fac_increases <- function(
    scrape_df = NULL, metric = "Residents.Confirmed", 
    plot_days = NULL, delta_days = 7, num_fac = 4, annotate = F) {
    
    if (is.null(scrape_df)) {
        scrape_df <- read_scrape_data(T, T)
    }
    
    plot_end_date <-  max(scrape_df$Date)
    delta_start_date <- plot_end_date - lubridate::days(delta_days)
    if (is.null(plot_days)) {
        plot_start_date <- min(scrape_df$Date)
    }
    else {
        plot_start_date <- plot_end_date - lubridate::days(plot_days)
    }
    
    fac_delta_df <- scrape_df %>%
        filter(!(str_detect(Name, "(?i)state") & str_detect(Name, "(?i)wide"))) %>%
        filter(Date >= delta_start_date) %>%
        group_by(Name, State) %>%
        mutate(delta = last(!!sym(metric)) - first(!!sym(metric))) %>%
        ungroup() %>%
        filter(delta %in% head(sort(unique(delta), decreasing = T), n = num_fac)) %>%
        select(State, Name) %>%
        unique() %>%
        left_join(scrape_df, by = c("State", "Name")) %>% 
        mutate(Title = str_c(Name, "\n", State, "\n")) %>%
        mutate(Title = str_to_title(Title)) %>% 
        mutate(last_value = if_else(Date == max(Date), as.character(!!sym(metric)), NA_character_))
    
    fac_delta_df %>%
        filter(Date >= plot_start_date) %>%
        filter(!is.na(!!sym(metric))) %>%
        ggplot(aes(x = Date, y = !!sym(metric), color = Title, label = last_value)) +
        geom_line(size = 2.0) +
        geom_point(size = 3.0, show.legend = F) +
        {if (annotate) 
            geom_label_repel(na.rm = T, show.legend = F, label.r = 0, size = 6)} + 
        labs(title = str_c("Facilities with Recent Spikes in ", get_metric_description(metric, short = T)),
             subtitle = get_metric_description(metric),
             x = "Date",
             y = get_metric_description(metric, short = T),
             color = "Facility",
             tag = str_to_upper("UCLA Law COVID-19\nBehind Bars Data Project\ncovid19behindbars.org")) +
        theme_behindbars() +
        theme(axis.text.y = element_text(vjust = -0.6, hjust = 0, margin = margin(r = -45))) +
        scale_color_bbdiscrete() +
        scale_x_date(limits = c(plot_start_date, plot_end_date)) 
}