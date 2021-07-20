#' Read meta-data on the last dates scraper data was update by agency
#'
#' Reads extracted data on the last time covid related data was updated from
#' files on our webserver.
#'
#' @param all_dates logical, get all data from all dates recorded by webscraper
#' @param scraper_name character, a character string indicating a particular
#' scraper to pull data for. Defaults to NULL which pulls all scraper data
#'
#' @return dataframe with days since last update
#'
#' @examples
#'
#' pull_last_update(all_dates = TRUE, scraper_name = "arizona")
#'
#' \dontrun{
#' # this takes a minute
#' last_df <- pull_last_update()
#'
#' last_df %>%
#'     filter(!is.na(days_late)) %>%
#'     # only get these kind of scrapers
#'     filter(jurisdiction %in% c("state", "federal", "immigration")) %>%
#'     # remove population scrapers here
#'     filter(!stringr::str_detect(id, "(?i)population")) %>%
#'     # remove youth scrapers here
#'     filter(!stringr::str_detect(id, "(?i)youth")) %>%
#'     # Do some renaming
#'     mutate(State = ifelse(jurisdiction == "immigration", "ICE", State)) %>%
#'     mutate(State = ifelse(jurisdiction == "federal", "BOP", State)) %>%
#'     # average across all scrapers for a given agency
#'     group_by(State) %>%
#'     summarize(days_late = mean(days_late), .groups = "drop") %>%
#'     mutate(State = forcats::fct_reorder(State, days_late)) %>%
#'     ggplot(aes(x = State, y = days_late, xend = State, yend = 0)) +
#'     geom_point(size = 3, color = "#D7790F") +
#'     geom_segment(size = 1.5, color = "#D7790F") +
#'     coord_flip() +
#'     theme_behindbars() +
#'     theme(
#'         panel.grid.major.y = element_blank(),
#'         axis.text.y = element_text(color = "#555526", size = 13),
#'         axis.text.x = element_text(color = "#555526", size = 18),
#'         panel.grid.major.x = element_line(
#'             color = "#92926C", linetype = "dotted"),
#'         axis.title.x = element_text(margin = margin(r = 20)),
#'         axis.title.y = element_blank(),
#'         legend.position = "none") +
#'     scale_color_bbdiscrete() +
#'     labs(x="", y="") +
#'     ggtitle(
#'         "Days Since Agency Updated Covid Data",
#'         stringr::str_c("As of ", as.character(first(last_df$Date))))
#' }
#'
#' @importFrom purrr map_dfr
#' @export

pull_last_update <- function(all_dates = FALSE, scraper_name = NULL){
    rem_files <- list_remote_data("last_update", scraper_name = scraper_name)

    rem_df <- tibble(
        Date = rem_files %>%
            stringr::str_extract("\\d{4}-\\d{2}-\\d{2}") %>%
            lubridate::ymd(),

        id = rem_files %>%
            stringr::str_split_fixed("\\d{4}-\\d{2}-\\d{2}_", 2) %>%
            .[,2] %>%
            stringr::str_remove("\\.csv"),

        loc = rem_files)

    if(!all_dates){
        rem_df <- rem_df %>%
            group_by(id) %>%
            filter(Date == max(Date)) %>%
            ungroup()
    }

    if(nrow(rem_df) == 1){
        out_df = readr::read_csv(rem_df$loc, col_types = readr::cols())
    } else {
        out_df = rem_df$loc %>%
            purrr::map_dfr(~ readr::read_csv(.), col_types = readr::cols())
    }

    return(out_df)
}
