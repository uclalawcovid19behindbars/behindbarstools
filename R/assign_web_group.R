#' Assigns UCLA website groupings 
#'
#' Assigns UCLA website groupings based on a combination of a Jurisdiction and 
#' Age field. Web groupings include ICE, Federal, Juvenile, Prison, Psychiatric, 
#' and County. ICE and Federal facilities will never be assigned Juvenile (i.e., 
#' Jurisdiction takes precedence for ICE and Federal facilities, and Age takes
#' precedence for State, County, and Psychiatric facilities). 
#' 
#' @param dat data frame with columns for Jurisdiction and Age (at least)
#'
#' @return original data frame with new Web.Group column added 
#'
#' @examples
#' assign_web_group(
#'     tibble(
#'         Name = "Sample youth facility", 
#'         Age = "Juvenile", 
#'         Jurisdiction = "state"
#'     ) %>% add_row(
#'         Name = "Sample federal facility", 
#'         Age = "Adult", 
#'         Jurisdiction = "federal"
#'         )
#' )
#' @export

assign_web_group <- function(dat){
    
    if(!"Jurisdiction" %in% colnames(dat)){
        stop("A column called Jurisdiction must be in data.")
    }
    
    if(!"Age" %in% colnames(dat)){
        stop("A column called Age must be in data.")
    }
    
    dat_with_web_group <- dat %>% 
        mutate(Web.Group = case_when(
            Jurisdiction == "immigration" ~ "ICE",
            Jurisdiction == "federal" ~ "Federal",
            Age == "Juvenile" ~ "Juvenile",
            Jurisdiction == "state" ~ "Prison",
            Jurisdiction == "psychiatric" ~ "Psychiatric",
            Jurisdiction == "county" ~ "County",
            TRUE ~ NA_character_
        ))
    
    return(dat_with_web_group)
}
