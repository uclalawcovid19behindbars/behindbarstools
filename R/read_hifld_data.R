#' Read in HIFLD Facility level Data
#'
#' Reads in data from HIFLD on facility information including population, adds
#' coordinates (latitude and longitude) for the centroid of each facility, replaces
#' -999 population and capacity values with NA.
#'
#' @return data frame with facility info and coordinates
#'
#' @import dplyr
#' @importFrom geojsonsf geojson_sf
#' @export
#'
#' @examples
#' \dontrun{
#' read_hifld_data()
#' }

read_hifld_data <- function(){
    "https://opendata.arcgis.com/datasets/2d6109d4127d458eaf0958e4c5296b67_0.geojson" %>%
        geojson_sf() %>%
        # we have to manually set the CRS here because of GDAL version issues
        # so ignoring the warnings is fine in this case
        {suppressWarnings(mutate(., geometry = sf::st_set_crs(geometry, 4326)))} %>%
        mutate(geometry = sf::st_transform(geometry, 2901),
               coords = sf::st_centroid(geometry),
               coords = sf::st_transform(coords, 4326)) %>%
        as_tibble() %>%
        mutate(lon = sf::st_coordinates(coords)[,1] %>% unname(),
               lat = sf::st_coordinates(coords)[,2] %>% unname()) %>%
        rename(hifld_id = FACILITYID) %>%
        mutate(POPULATION = na_if(POPULATION, -999),
               CAPACITY = na_if(CAPACITY, -999)) %>%
        select(hifld_id,
               NAME,
               ADDRESS,
               CITY,
               STATE,
               ZIP,
               TELEPHONE,
               TYPE,
               STATUS,
               POPULATION,
               COUNTY,
               COUNTYFIPS,
               COUNTRY,
               NAICS_CODE,
               NAICS_DESC,
               SOURCE,
               SOURCEDATE,
               VAL_METHOD,
               VAL_DATE,
               WEBSITE,
               SECURELVL,
               CAPACITY,
               lat,
               lon)
}
