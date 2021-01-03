#' Plot a single facility metric
#'
#' Creates time series plots showing the trend for a single facility for a given
#' metric, formatted according to the style guide. Returns a ggplot object, so
#' additional layers can be added, per the examples.
#'
#' @param fac_name character string of the facility to plot
#' @param state character string of the facility's state
#' @param metric character string of the metric to plot
#' @param scrape_df data frame with scraped data
#' @param plot_days integer, number of days to plot
#' @param annotate logical, whether to include text annotations for most recent value
#' @param auto_label logical, whether to label plot title, subtitle, axes based on behindbarstools::get_metric_description
#'
#' @return ggplot object
#'
#' @examples
#' \dontrun{
#' plot_fac_trend("Los Angeles Jails", "California", "Residents.Active")
#' }
#'
#' \dontrun{
#' plot_fac_trend("Los Angeles Jails", "California", "Residents.Active") +
#'     scale_x_date(date_labels = "%m/%y") +
#'     labs(x = "Cases") +
#'     ylim(0, 150)
#' }
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom stringr str_c
#' @importFrom stringr str_to_upper
#'
#' @export

plot_fac_trend <- function(
    fac_name, state, metric, scrape_df = NULL, plot_days = NULL, annotate = F, auto_label = F) {

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
            ggrepel::geom_text_repel(na.rm = T, show.legend = F, size = 6, nudge_x = 1, point.padding = 0.1)} +
        {if (auto_label)
            labs(title = str_c(get_metric_description(metric, short = T), " in ", fac_name),
                 subtitle = get_metric_description(metric),
                 x = "Date",
                 y = get_metric_description(metric, short = T),
                 caption = str_to_upper("UCLA Law COVID-19 Behind Bars Data Project"))} +
        scale_x_date(date_labels = "%b %d",
                     limits = c(plot_start_date, plot_end_date),
                     expand = c(0.15, 0)) +
        scale_color_bbdiscrete() +
        theme_behindbars() +
        theme(legend.position = "none",
              axis.title.x = element_blank())
}
