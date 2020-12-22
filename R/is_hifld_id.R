#' Returns TRUE if the given ID is a valid HIFLD ID 
#' 
#' Returns TRUE if the given ID is a valid HIFLD, includes a parameter to 
#' return FALSE vs. NA if the given ID is NA. 
#'
#' @param id integer or character string of the ID to check 
#' @param hifld_data data frame with HIFLD information 
#' @param na_is_na logical, whether NA should return NA (vs. FALSE)
#'
#' @return logical, TRUE if the given ID is a valid HIFLD ID  
#' @export
#'
#' @examples
#' is_hifld_id(10002598) 
#' is_hifld_id("10002598")

is_hifld_id <- function(id, hifld_data = NULL, na_is_na = T) {
    
    if (is.na(id) & na_is_na) {return(NA)}
    
    if (is.null(hifld_data)) {
        hifld_data <- behindbarstools::read_hifld_data()
    }
    
    valid_ids <- hifld_data %>% 
        select(hifld_id) %>% 
        distinct() %>% 
        unlist()
    
    rv <- id %in% valid_ids
    
    if (!rv) {warning(paste0(id, " is not a valid HIFLD."))}
    
    return(rv) 
}
