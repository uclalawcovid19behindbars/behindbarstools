#' Plot facilities with recent COVID-19 metric increases  
#'
#' Creates time series plots showing trends for facilities with the steepest recent 
#' increases in a given metric, formatted according to the style guide. Returns a 
#' ggplot object, so additional layers can be added, per the examples. 
#'
#' @param scrape_df data frame with scraped data 
#' @param metric character string of the metric to plot 
#' @param delta_days integer, number of days to calculate increase over  
#' @param plot_days integer, number of days to plot  
#' @param num_fac integer, number of facilities to plot 
#' @param annotate logical, whether to include text annotations for most recent value 
#' @param auto_label logical, whether to label plot title, subtitle, axes based on behindbarstools::get_metric_description
#' 
#' @return ggplot object 
#' 
#' @examples
#' \dontrun{
#' plot_recent_fac_increases()
#' plot_recent_fac_increases(metric = "Residents.Deaths", plot_days = 30, num_fac = 3)
#' }
#' 
#' \dontrun{
#' plot_recent_fac_increases() + 
#'     labs(y = "Cases Among Incarcerated People") + 
#'     ylim(0, 3000)
#' }
#' 
#' @import ggplot2
#' @import dplyr 
#'
#' @export

plot_recent_fac_increases <- function(
    scrape_df = NULL, metric = "Residents.Confirmed", 
    plot_days = NULL, delta_days = 7, num_fac = 4, annotate = F, auto_label = F) {
    
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
    
    keep_facs <- scrape_df %>%
        filter(!(stringr::str_detect(Name, "(?i)state") & stringr::str_detect(Name, "(?i)wide"))) %>%
        filter(Date >= delta_start_date) %>%
        group_by(Name, State) %>%
        mutate(delta = last(!!sym(metric)) - first(!!sym(metric))) %>%
        distinct(Name, State, delta) %>% 
        arrange(desc(delta), Name) %>% # Use name as a tie-breaker 
        head(num_fac) 
        
    fac_delta_df <- keep_facs %>% 
        left_join(scrape_df, by = c("State", "Name")) %>% 
        mutate(Title = stringr::str_c(Name, "\n", State, "\n")) %>%
        mutate(last_value = if_else(Date == max(Date), as.character(!!sym(metric)), NA_character_))
    
    fac_delta_df %>%
        filter(Date >= plot_start_date) %>%
        filter(!is.na(!!sym(metric))) %>%
        ggplot(aes(x = Date, y = !!sym(metric), color = Title, label = last_value)) +
        geom_line(size = 2.0) +
        geom_point(size = 3.0) +
        {if (annotate) 
            ggrepel::geom_text_repel(na.rm = T, show.legend = F, size = 6, nudge_x = 1, point.padding = 0.1)} + 
        {if (auto_label) 
            labs(title = stringr::str_c("Facilities with Recent Spikes in ", get_metric_description(metric, short = T)),
                 subtitle = get_metric_description(metric),
                 y = get_metric_description(metric, short = T),
                 color = "Facility",
                 tag = stringr::str_to_upper("UCLA Law COVID-19\nBehind Bars Data Project\ncovid19behindbars.org"))} +
        scale_y_continuous(labels = scales::comma) + 
        scale_x_date(date_labels = "%b %d", 
                     limits = as.Date(c(plot_start_date, plot_end_date)),  
                     expand = c(0.15, 0)) + 
        scale_color_bbdiscrete() + 
        theme_behindbars() +
        theme(axis.text.y = element_text(vjust = -0.6, hjust = 0, margin = margin(r = -45)), 
              legend.key.width = unit(1, "cm")) 
}
