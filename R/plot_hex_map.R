# TODO: What should the function take and return? 
# Take: df w/ state or state abbreviation variable? 
# Return: ggplot object? 

plot_hex_map <- function() {
    spdf <- geojsonio::geojson_read(
        "https://raw.githubusercontent.com/uclalawcovid19behindbars/data/master/README_files/us_states_hexgrid.geojson", 
        what = "sp")
    
    spdf@data = spdf@data %>% 
        mutate(google_name = gsub(" \\(United States\\)", "", google_name))
    
    spdf_fortified <- broom::tidy(spdf, region = "google_name") %>% 
        mutate(state_abb = behindbarstools::translate_state(id, reverse = TRUE), 
               state_abb = ifelse(id == "District of Columbia", "DC", state_abb))
    
    centers <- cbind.data.frame(data.frame(
        rgeos::gCentroid(spdf, byid = TRUE), 
        id = spdf@data$iso3166_2)) 
    
    ggplot() + 
        geom_polygon(data = spdf_fortified, aes(x = long, y = lat, group = group), 
                     fill = "#D7790F", 
                     color = "white") + 
        geom_text(data = centers, aes(x = x, y = y, label = id)) + 
        theme_map_behindbars() + 
        coord_map() 
}