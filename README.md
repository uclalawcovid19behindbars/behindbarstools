
[![logo](README_files/logo.svg)](https://uclacovidbehindbars.org/)

# behindbarstools

[![R build status](https://github.com/uclalawcovid19behindbars/behindbarstools/workflows/R-CMD-check/badge.svg)](https://github.com/uclalawcovid19behindbars/behindbarstools/actions)

## Overview

`behindbarstools` is an R package with the set of data tools used by the
[UCLA Law COVID-19 Behind Bars
Project](https://uclacovidbehindbars.org/) – a data project that
collects and reports facility-level data on COVID-19 in jails, prisons,
and other carceral facilities. `behindbarstools` includes a variety of
functions to help pull, clean, wrangle, and visualize our data.

**Warning:** This package is actively under development.

## Installation

``` r
# Install directly from GitHub 
devtools::install_github("uclalawcovid19behindbars/behindbarstools")
```

## Usage Examples

### Reading Data

The `read_scrape_data()` function can be used to load our data.

`behindbarstools` also includes functions to more easily load related
data from other organizations including [the Vera Institute’s Jail
Population Data](https://github.com/vera-institute/jail-population-data)
through `read_vera_pop()` and [the Department of Homeland Security’s
Homeland Infrastructure Foundation-Level
Data](https://hifld-geoplatform.opendata.arcgis.com/datasets/prison-boundaries/data)
through `read_hifld_data()`.

``` r
library(behindbarstools)

# Pull latest data
latest_scraped <- read_scrape_data()

# Pull historical scraped data for California 
scraped_CA <- read_scrape_data(all_dates = TRUE, 
                               state = "California")
```

### Processing Data

The majority of the functions in `behindbarstools` help standardize our
ETL and data cleaning process. This includes functions to help with the
following:

  - Cleaning facility names, e.g.`clean_fac_col_txt()`,
    `clean_facility_name()`
  - Coalescing data from various sources,
    e.g. `coalesce_with_warnings()`, `group_by_coalesce()`
  - Enforcing data validation, e.g. `is_valid_state()`, `is_federal()`
  - Standardizing our data scraping infrastructure,
    e.g. `ExtractTable()`, `get_src_by_attr()`

See [our package
documentation](https://github.com/uclalawcovid19behindbars/behindbarstools/tree/master/man)
for more information and examples for each function.

### Visualizing Data

`behindbarstools` also includes functions to create data visualizations.
This includes a custom `ggplot2` theme called `theme_behindbars()` that
incorporates our team’s style guide. All plotting functions return
[`ggplot` objects](https://ggplot2.tidyverse.org/reference/ggplot.html),
making it easy to customize and add additional layers.

``` r
# Plot cumulative COVID-19 cases in the Los Angeles Jails over the past 30 days  
plot_fac_trend(fac_name = "Los Angeles Jails", 
               state = "California", 
               metric = "Residents.Confirmed", 
               plot_days = 30, 
               auto_label = TRUE) + 
    theme_behindbars(base_size = 14) + 
    ggplot2::ylim(3500, 4000) + 
    ggplot2::theme(legend.position = "none")
```

![](README_files/figure-gfm/plot-fac-1.png)<!-- -->

``` r
# Plot the 3 facilities with the largest recent spikes in active COVID-19 cases  
plot_recent_fac_increases(metric = "Residents.Active",
                          plot_days = 60, 
                          num_fac = 3, 
                          auto_label = TRUE) + 
    theme_behindbars(base_size = 14) + 
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), 
                   plot.tag.position = c(0.80, 0.05))
```

![](README_files/figure-gfm/plot-spikes-1.png)<!-- -->
