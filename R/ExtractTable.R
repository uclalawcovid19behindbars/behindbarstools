#' Parse API response from extractable
#'
#' @param server_resp response from httr
#' @return extracted list results from json
#'
#' @importFrom httr content
#' @importFrom jsonlite fromJSON

parse_response <- function(server_resp){
    jsonlite::fromJSON(httr::content(server_resp, "text", encoding = "UTF-8"))
}

#' Retrieve result from API endpoint
#'
#' @param api_key character of extractable api key
#' @param job_id character of job id to pull
#' @return raw extracted results
#'
#' @importFrom httr GET
#' @importFrom httr add_headers
retrieve_result <- function(api_key, job_id) {
    retrieve_endpoint = "https://getresult.extracttable.com"
    return(
        httr::GET(
            url = paste0(retrieve_endpoint, "/?JobId=", job_id),
            httr::add_headers(`x-api-key` = api_key)
        )
    )
}

#' Post image for extraction from API endpoint
#'
#' @param api_key character of extractable api key
#' @param filepath character to image filepath to post
#'
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @importFrom httr upload_file
process_file <- function(api_key, filepath) {
    trigger_endpoint = "https://trigger.extracttable.com"
    return (
        httr::POST(
            url = trigger_endpoint,
            httr::add_headers(
                `Content-Type`="multipart/form-data", `x-api-key` = api_key),
            body = list(input = httr::upload_file(filepath))
        )
    )
}

#' Function to extract all tables from an image
#'
#' Extracts tables from an image, returns a list of matrices and vectors with
#' extracted information.
#'
#' @param img character indicating file location of image or an object of
#' class 'magick-image'
#' @param file_type character image type to save magick image to temp file for
#' extraction
#' @param api_key character extractable api key. If NULL will search Environment
#' variables for object 'EXTRACTABLE_API_KEY'
#' @return list of matrices and vectors of extracted table information
#'
#' @importFrom magick image_write
#' @importFrom httr content
#' @importFrom httr upload_file
#' @export
#'
#' @examples
#'
#' "https://dps.hawaii.gov/wp-content/uploads/2020/03/" %>%
#'     paste0("Inmate-Test-Report-12.1.20.jpg") %>%
#'     magick::image_read() %>%
#'     ExtractTable()

ExtractTable <- function(img,  api_key = NULL) {
    if(is.null(api_key)){
        api_key <- Sys.getenv("EXTRACTABLE_API_KEY")
    }

    # TODO: this cant be the right way to check this? right
    if(class(img) == "magick-image"){
        f_ <- tempfile(fileext = ".png")
        magick::image_write(img, f_)
        img <- f_
    }

    server_response <- process_file(api_key, img)
    parsed_resp <- parse_response(server_response)
    # Wait for a maximum of 5 minutes to finish the trigger job
    # Retries every 20 seconds
    max_wait_time = 5*60
    retry_interval = 20
    while (parsed_resp$JobStatus == 'Processing' & max_wait_time >= 0) {
        max_wait_time = max_wait_time - retry_interval
        message(paste0(
            "Job is still in progress. Let's wait for ", retry_interval, " seconds"))
        Sys.sleep(retry_interval)
        server_response <- retrieve_result(api_key, job_id=parsed_resp$JobId)
        parsed_resp <- parse_response(server_response)
    }
    ### Parse the response for tables
    et_tables <- httr::content(
        server_response, as = 'parsed', type = 'application/json')
    all_tables <- list()
    if (tolower(parsed_resp$JobStatus) != "success") {
        print(paste0("The processing was NOT SUCCESSFUL Below is the complete response from the server"))
        print(parsed_resp)
        return(all_tables)
    }
    ### Convert the extracted tabular JSON data as a dataframe for future use
    ### Each data frame represents one table
    for (i in 1:length(et_tables$Table)) {
        all_tables[[i]] <- sapply(et_tables$Tables[[i]]$TableJson, unlist) %>%
            t() %>% as.data.frame()
    }
    return(all_tables)
}
