#' Returns a tibble of possible facility names in the facility crosswalks
#'
#' Function is case-insensitive, and will return partial matches from raw and .
#'    clean xwalk
#'
#' @param fac_name character string of the facility name
#' @param state character string of the facility's state
#'
#' @examples
#' match_fac_name_includes("MCDO", "West Virginia")
#'
#' @importFrom dplyr distinct
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom data.table as.data.table
#'
#' @export

library(memoise)

# Expire items in cache after 15 minutes
cm <- cachem::cache_mem(max_age = 15 * 60)

memoise(read_fac_info, cache = cm)
memoise(read_fac_spellings, cache = cm)

fac_info <- read_fac_info()
fac_spellings <- read_fac_spellings()

match_fac_name_includes <- function(
    fac_name, state) {
    if(state == 'NA'){
        state <- 'Not Available'
    }

    fac_data_names <- fac_info %>%
        select(Name, State) %>%
        distinct() %>%
        mutate(Type = 'Clean')

    # include alternate spellings
    fac_spelling_names <- fac_spellings %>%
        select(xwalk_name_raw, State) %>%
        distinct() %>%
        rename(Name = xwalk_name_raw) %>%
        mutate(Type = 'raw')

    all_names <- rbind(fac_data_names, fac_spelling_names)

    names_in_state <- all_names %>%
        filter(State == state) %>%
        distinct() %>%
        as.data.table()

    search_text <- str_c('*', fac_name, '*')
    matches <- names_in_state[Name %ilike% search_text]

    return(matches)

}
