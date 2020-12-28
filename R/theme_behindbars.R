#' Theme and colors for behind bars plots
#'
#' @description a set of themes and colors for behind bars plots
#'
#' @import ggplot2
#' @export
#'
#' @param base_size base font size, given in pts 
#' @param base_family base font family
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

theme_behindbars <- function(
    base_size = 24, base_family = "Helvetica") {
    
    ggplot2::theme_classic(
        base_family = base_family, 
        base_size = base_size
    ) +
        ggplot2::theme(
            text =                element_text(color = "#555526"), 
            strip.text =          element_text(color = '#555526'), 
            axis.text =           element_text(color = "#555526"), 
            panel.grid.major.y =  element_line(color = "#92926C", linetype = "dotted"),
            plot.title.position = "plot", 
            plot.tag.position =   "bottomright", 
            axis.line.y =         element_blank(),
            axis.ticks.y =        element_blank(), 
            axis.title.x =        element_blank(), 
            axis.line =           element_line(color = "#555526"), 
            axis.ticks =          element_line(color = "#555526"), 
            plot.caption =        element_text(margin = margin(t = 1.2 * base_size)), 
            plot.subtitle =       element_text(margin = margin(b = 1.2 * base_size)), 
            axis.title.y =        element_text(margin = margin(r = 1.2 * base_size)), 
            plot.tag =            element_text(size = base_size / 2, hjust = 0)
        )
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