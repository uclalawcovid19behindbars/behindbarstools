#' Read in HIFLD Facility level Data
#'
#' Reads in data from HIFLD on facility information including population, adds 
#' coordinates (latitude and longitude) for the centroid of each facility.
#'
#' @return data frame with facility info and coordinates 
#'
#' @import dplyr 
#' @export
#' 
#' @examples 
#' read_hifld_data()

read_hifld_data <- function(){
    "https://opendata.arcgis.com/datasets/2d6109d4127d458eaf0958e4c5296b67_0.geojson" %>% 
        geojsonsf::geojson_sf() %>% 
        mutate(geometry = sf::st_set_crs(geometry, 4326), 
               geometry = sf::st_transform(geometry, 2901), 
               coords = sf::st_centroid(geometry), 
               coords = sf::st_transform(coords, 4326)) %>% 
        as_tibble() %>% 
        mutate(lat = sf::st_coordinates(coords)[,1] %>% unname(),
               lon = sf::st_coordinates(coords)[,2] %>% unname()) %>% 
        rename(hifld_id = FACILITYID) %>% 
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
