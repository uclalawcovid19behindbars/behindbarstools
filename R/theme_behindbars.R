#' Theme and colors for behind bars plots
#'
#' @description a set of themes and colors for behind bars plots
#'
#' @import ggplot2
#' @export
#'
#' @examples
#' library(ggplot2)
#' iris %>%
#'     ggplot(aes(x = Petal.Width, y = Petal.Length, color = Species)) +
#'     geom_point() +
#'     theme_behindbars() +
#'     scale_color_bbdiscrete()
#'
#' iris %>%
#'     ggplot(aes(x = Petal.Width, fill = Species)) +
#'     geom_density() +
#'     theme_behindbars() +
#'     scale_fill_bbdiscrete()
#'
#' iris %>%
#'     ggplot(aes(x = Petal.Width, y = Petal.Length, color = Petal.Width)) +
#'     geom_point() +
#'     theme_behindbars() +
#'     scale_color_bbcontinous()

theme_behindbars <- function(){
    ggplot2::theme_classic() +
        ggplot2::theme(
            panel.grid.major.y = ggplot2::element_line(
                color = "#92926C",
                linetype = 9),
            axis.line.y = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank(),
            axis.line.x = ggplot2::element_line(
                color = "#555526"),
            axis.title.x = ggplot2::element_text(
                family = "Arial", size = 27, color = "#555526"),
            axis.title.y = ggplot2::element_text(
                family = "Arial", size = 27, color = "#555526"),
            legend.text = ggplot2::element_text(
                family = "Arial", size = 22, color = "#555526"),
            legend.title = ggplot2::element_text(
                family = "Arial", size = 22, color = "#555526"),
            axis.text.x = ggplot2::element_text(
                family = "Arial", size = 22, color = "#555526"),
            axis.text.y = ggplot2::element_text(
                family = "Arial", size = 22, color = "#555526"))
}


#' @import ggplot2
#' @export
#' @rdname theme_behindbars

scale_color_bbdiscrete <- function(){
    ggplot2::discrete_scale(
        aesthetics = "colour", scale_name = "bbdiscrete",
        palette = function(n) {
            if (n > 6) stop("bbcolors palette only has 6 colors.")

            bbcolors <- c(
                "#D7790F", "#82CAA4", "#4C6788", "#84816F",
                "#71A9C9", "#AE91A8")
            bbcolors[1:n]
        })
}

#' @import ggplot2
#' @export
#' @rdname theme_behindbars

scale_fill_bbdiscrete <- function(){
    ggplot2::discrete_scale(
        aesthetics = "fill", scale_name = "bbdiscrete",
        palette = function(n) {
            if (n > 6) stop("bbcolors palette only has 6 colors.")

            bbcolors <- c(
                "#D7790F", "#82CAA4", "#4C6788", "#84816F",
                "#71A9C9", "#AE91A8")
            bbcolors[1:n]
        })
}

#' @import ggplot2
#' @export
#' @rdname theme_behindbars

scale_color_bbcontinous <- function(){
    ggplot2::scale_colour_gradient(
        low = "#71A9C9",
        high = "#bd2828",
        na.value = "grey50",
        guide = "colourbar",
        aesthetics = "colour"
    )
}

#' @import ggplot2
#' @export
#' @rdname theme_behindbars

scale_fill_bbcontinous <- function(){
    ggplot2::scale_fill_gradient(
        low = "#71A9C9",
        high = "#bd2828",
        na.value = "grey50",
        guide = "colourbar",
        aesthetics = "colour"
    )
}
