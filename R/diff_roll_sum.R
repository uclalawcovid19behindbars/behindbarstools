# rolling sum window for n days
rsum.cumsum <- function(x, n = 3L){
    if(length(x) <= n ){
        return(rep(NA_real_, length(x)))
    }
    out <- cumsum(x) - cumsum(c(rep(0, n), head(x, -n)))
    out[1:(n-1)] <- NA
    return(out)
}

#' A function to calculate a rolling sum of first differences
#'
#' Calculates a rolling sum of first differences on a vector, or optionally and
#' recommended, calculates first differences and rolling sum by day where
#' missing day values are treated as the same as the last observed value.
#'
#' @param x a numeric vector
#' @param date_vec date, vector of dates of equal length to x default NULL
#' @param window int, integer indicating rolling sum window length
#'
#' @examples
#' \dontrun{
#' read_scrape_data(all_dates = TRUE, state = "North Carolina") %>%
#'     group_by(Name, Jurisdiction) %>%
#'     # calculate values by name and jurisdication for all of state of NC
#'     mutate(Res.Act.Est = diff_roll_sum(Residents.Confirmed, Date)) %>%
#'     # filter here only for the example plot
#'     filter(Name == "NORTH CAROLINA CORRECTIONAL INSTITUTION FOR WOMEN") %>%
#'     ggplot(aes(
#'         x = Date, y = Res.Act.Est, color = Name, fill = Name)) +
#'     geom_line(size = 1.5) +
#'     geom_area(alpha = .5) +
#'     theme_behindbars() +
#'     scale_color_bbdiscrete() +
#'     scale_fill_bbdiscrete() +
#'     theme(legend.position = "none") +
#'     labs(y = "Estimated Active Cases") +
#'     ggtitle("Monitoring Facility Outbreaks")
#' }
#'
#' # example of difference using position vs using dates for rolling sum diffs
#' x <- c(1, 1, 4, 4, 7, 9)
#' sd <- Sys.Date()
#' date_x <- sd - c(10, 9, 8, 6, 4, 2)
#'
#' diff_roll_sum(x, window = 2)
#' \dontrun{
#' # returns a warning
#' diff_roll_sum(x, date_x, window = 2)
#' }
#'
#' @import dplyr
#' @export

diff_roll_sum <- function(x, date_vec = NULL, window = 14){

    if(length(window) > 1 | !is.numeric(window)){
        stop("window should be a single numeric value of length one")
    }

    if(all(is.na(x))){
        return(rep(NA_real_, length(x)))
    }

    if(is.null(date_vec)){
        z <- c(NA, diff(x))
        return(rsum.cumsum(ifelse(is.na(z), 0, z), window))
    }

    if(class(date_vec) != "Date"){
        stop("date_vec must be of class Date")
    }

    if(any(is.na(date_vec))){
        stop("date_vec values should not be NA")
    }

    if(length(unique(date_vec)) != length(date_vec)){
        stop("date_vec should not contain repeat values")
    }

    if(any(as.numeric(diff(date_vec)) < 1)){
        stop("date_vec column should be ordered")
    }

    if(length(date_vec) != length(x)){
        stop("x and date_vec should be of equal length")
    }

    og_df <- tibble(Confirmed = x, Date = date_vec)

    ex_df <- og_df %>%
        # arrange by date
        arrange(Date) %>%
        # remove values where x column is NA
        filter(!is.na(Confirmed)) %>%
        # compute lag differences
        mutate(New.Cases = Confirmed - lag(Confirmed)) %>%
        # calculate the change in date
        mutate(Delta = as.numeric(Date - lag(Date))) %>%
        right_join(
            tibble(Date = seq(min(.$Date), max(.$Date), 1)), by = "Date") %>%
        # arrange by date again because merge messes things up
        arrange(Date) %>%
        # change first diffs of NA to zero
        mutate(New.Cases = ifelse(is.na(New.Cases), 0, New.Cases)) %>%
        # get the cum sum of differences
        mutate(Estimated.Active = rsum.cumsum(New.Cases, n = window))

    if(max(ex_df$Delta, na.rm = TRUE) > window/2){
        warning(
            "Because of inconsitent dates of values in x",
            "results taken with caution.")
    }

    est_x <- og_df %>%
        left_join(select(ex_df, Date, Estimated.Active), by = "Date") %>%
        pull(Estimated.Active)

    return(est_x)
}
