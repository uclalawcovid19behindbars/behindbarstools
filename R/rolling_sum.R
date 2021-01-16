# rolling sum window for n days
rsum.cumsum <- function(x, n = 3L) {
    out <- cumsum(x) - cumsum(c(rep(0, n), head(x, -n)))
    out[1:(n-1)] <- NA
    out
}

#' A function to add in a rolling sum estiomate column by groups
#'
#' Adds in a rolling sum calculation to a data frame which can be used as a way
#' to estimate active cases from confirmed cases.
#'
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy
#' data frame (e.g. from dbplyr or dtplyr). Must have a date column of class
#' date
#' @param ... one or more unquoted expressions separated by commas. Variable
#' names can be used as if they were positions in the data frame,
#' so expressions like x:y can be used to select a range of variable. These
#' columns should uniquely identify rows in the data
#' @param .col character, the column to perform the rolling sum on
#' @param .window integer, the window of retrospective days for rolling sum
#' @param .new_col character, the new column name to be added on
#' @return data frame with new column with rolling sum added
#'
#' @examples
#' \dontrun{
#' read_scrape_data(all_dates = TRUE, state = "North Carolina") %>%
#'     filter(Name == "NORTH CAROLINA CORRECTIONAL INSTITUTION FOR WOMEN") %>%
#'     rolling_sum(Name) %>%
#'     ggplot(aes(
#'         x = Date, y = Residents.Active.Estimate, color = Name, fill = Name)) +
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
#' @import dplyr
#' @export

rolling_sum <- function(
    .data, ..., .col = "Residents.Confirmed", .window = 14,
    .new_col = "Residents.Active.Estimate"){

    if(!("Date" %in% names(.data))){
        stop(".data must have a column named date of class Date.")
    }

    if(class(.data$Date) != "Date"){
        stop(".data must have a column named date of class Date.")
    }

    dots <- dplyr::enquos(...)

    check_df <- .data %>%
        select(!!!dots, Date) %>%
        distinct()

    if(nrow(check_df) != nrow(.data)){
        stop(
            "Columns provided in ... along with date should indicate distinct",
            "rows")
    }

    .data[["to_manip"]] <- .data[[.col]]

    lag_df <- .data %>%
        select(!!!dots, Date, to_manip) %>%
        arrange(!!!dots, Date) %>%
        filter(!is.na(Date)) %>%
        filter(!is.na(to_manip)) %>%
        group_by(!!!dots) %>%
        mutate(new_manip = to_manip - lag(to_manip)) %>%
        mutate(delta_date = as.numeric(Date - lag(Date))) %>%
        filter(!is.na(new_manip))

    if(any(lag_df$delta_date > (.window / 2), na.rm = TRUE)){
        warning(
            "Because of inconsitent reporting these est imates should be",
            "taken with caution.")
    }

    group_df <- .data %>%
        select(!!!dots) %>%
        distinct()

    date_df <- bind_rows(lapply(1:nrow(group_df), function(i){
        spec_df <- left_join(group_df[i,], lag_df)

        group_df[i,] %>%
            bind_cols(tibble(
                Date = seq(min(spec_df$Date), max(spec_df$Date), by = 1)))
    }))

    roll_df <- date_df %>%
        left_join(ungroup(lag_df)) %>%
        mutate(new_manip = ifelse(is.na(new_manip), 0, new_manip)) %>%
        arrange(!!!dots, Date) %>%
        group_by(!!!dots) %>%
        mutate(roll_sum = rsum.cumsum(new_manip, n = .window)) %>%
        ungroup() %>%
        select(!!!dots, Date, roll_sum)

    out_df <- left_join(.data, roll_df) %>%
        select(-to_manip)
    out_df[[.new_col]] <- out_df[["roll_sum"]]
    out_df[["roll_sum"]] <- NULL

    return(out_df)
}

