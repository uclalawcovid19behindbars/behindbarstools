% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/theme_behindbars.R
\name{theme_behindbars}
\alias{theme_behindbars}
\alias{theme_map_behindbars}
\alias{scale_color_bbdiscrete}
\alias{scale_fill_bbdiscrete}
\alias{scale_color_bbcontinous}
\alias{scale_fill_bbcontinous}
\title{Theme and colors for behind bars plots}
\usage{
theme_behindbars(
  base_size = 24,
  base_family = "Helvetica",
  base_color = "black"
)

theme_map_behindbars(base_size = 24, base_family = "Helvetica")

scale_color_bbdiscrete()

scale_fill_bbdiscrete()

scale_color_bbcontinous()

scale_fill_bbcontinous()
}
\arguments{
\item{base_size}{base font size, given in pts}

\item{base_family}{base font family}

\item{base_color}{base color for text and axis lines}
}
\description{
a set of themes and colors for behind bars plots
}
\examples{
\dontrun{
library(ggplot2)
iris \%>\%
    ggplot(aes(x = Petal.Width, y = Petal.Length, color = Species)) +
    geom_point() +
    theme_behindbars() +
    scale_color_bbdiscrete()

iris \%>\%
    ggplot(aes(x = Petal.Width, fill = Species)) +
    geom_density() +
    theme_behindbars() +
    scale_fill_bbdiscrete()

iris \%>\%
    ggplot(aes(x = Petal.Width, y = Petal.Length, color = Petal.Width)) +
    geom_point() +
    theme_behindbars() +
    scale_color_bbcontinous()

usmap::plot_usmap() +
    theme_map_behindbars()
    }
}
