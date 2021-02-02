#' Get general population COVID data from NYT and ACS 2018
#'
#' @description Pulls data from the NYT github page on COVID cases and deaths
#' for a county and then merges alongside a population number from the 2018
#' ACS for that county to use for rate calculation.
#'
#' @param county either a 5 digit fips code (safer) or a county name
#' @param state a state name or abbreviation if a county name was provided
#' @return a data frame with the following columns: Date, County, State, FIPS,
#' General.Confirmed, General.Deaths, General.Population2018
#'
#' @examples
#' \dontrun{
#' # get data from Orange county, North Carolina
#' get_genpop_covid(county = "Orange", state = "NC")
#' # get data from los angeles by fips code
#' get_genpop_covid(county = "06037")
#'
#' get CA hist data
#' ca_df <- read_scrape_data(TRUE, state = "California")
#'
#' # look only at SATF
#' satf_df <- ca_df %>%
#'     filter(Name == "SUBSTANCE ABUSE TREATMENT FACILITY")
#'
#' # get the corresponding county data
#' county_df <- get_genpop_covid(first(satf_df$County.FIPS))
#'
#' # make the plot comparing prison vs general population
#' county_df %>%
#'     # get rid of potential conflicting columns
#'     select(Date, tidyr::starts_with("General")) %>%
#'     right_join(satf_df) %>%
#'     mutate(`Prisoner\nPopulation` = Residents.Confirmed / Population.Feb20) %>%
#'     mutate(`King County\nPopulation` =
#'                General.Confirmed / General.Population2018) %>%
#'     select(Date, `Prisoner\nPopulation`, `King County\nPopulation`) %>%
#'     tidyr::pivot_longer(-Date) %>%
#'     mutate(name = forcats::fct_rev(name)) %>%
#'     ggplot(aes(x = Date, y = value, color = name, fill = name)) +
#'     geom_area(alpha=.5, size = 1.5, position = position_dodge()) +
#'     theme_behindbars() +
#'     scale_color_bbdiscrete() +
#'     labs(y = "Proportion\nInfected", color = "", fill = "") +
#'     ylim(c(0,.65)) +
#'     ggtitle(
#'         "Substance Abuse Treatment Facility Outbreak",
#'         "Comparing COVID Outbreaks in Prison and the Surrounding Area"
#'     )
#'
#' }
#'
#' @export

get_genpop_covid <- function(county, state = NULL){
    county_source <- "https://raw.githubusercontent.com/nytimes/" %>%
        paste0("covid-19-data/master/us-counties.csv")

    county_df <- readr::read_csv(county_source, col_types = readr::cols()) %>%
        rename(
            "Date" = "date",
            "County" = "county",
            "State" = "state",
            "FIPS" = "fips",
            "General.Confirmed" = "cases",
            "General.Deaths" = "deaths"
        )

    if(is.numeric(county)){
        county <- sprintf("%05d", county)
    }

    if(grepl("^[[:digit:]]", county)){
        covid_df <- filter(county_df, FIPS == county)
    }

    else{
        if(is.null(state)){
            stop("Unless a fips code is provided state name cant be NULL")
        }
        if(stringr::str_count(state) == 2){
            state <- translate_state(stringr::str_to_upper(state))
        }

        covid_df <- county_df %>%
            filter(stringr::str_to_title(county) == County) %>%
            filter(State == stringr::str_to_title(state))
    }

    if(nrow(covid_df) == 0){
        return(tibble())
    }

    # read 2018 acs county pop totals
    pop_list <- "https://datausa.io/api/data?drilldowns=County&" %>%
        paste0("measures=Population&year=2018") %>%
        jsonlite::read_json(simplifyVector = TRUE)

    out_df <- pop_list$data %>%
        filter(stringr::str_ends(`ID County`, first(covid_df$FIPS))) %>%
        pull(Population) %>%
        {mutate(covid_df, General.Population2018 = .)}

    return(out_df)
}
