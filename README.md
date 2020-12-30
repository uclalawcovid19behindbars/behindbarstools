[![logo](logo.svg)](https://uclacovidbehindbars.org/)

# behindbarstools

[![Travis build status](https://travis-ci.com/uclalawcovid19behindbars/behindbarstools.svg?branch=master)](https://travis-ci.com/uclalawcovid19behindbars/behindbarstools)

## Overview
`behindbarstools` is an R package with the set of data tools used by the [UCLA Law COVID-19 Behind Bars Project](https://uclacovidbehindbars.org/) – a data project that collects and reports facility-level data on COVID-19 in jails, prisons, and other carceral facilities. `behindbarstools` includes a variety of functions to help pull, clean, wrangle, and visualize our data. 

**Warning:** This package is actively under development. 

## Installation 
```
devtools::install_github("uclalawcovid19behindbars/behindbarstools")
```

This package uses [renv](https://rstudio.github.io/renv/) to ensure that our environment is reproducible. 
```
# Recreate the package environment 
renv::restore()

# Update the lockfile after installing or updating packages 
renv::snapshot()
```
