#' @importFrom readr read_csv
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes
#' @importFrom rvest html_table
#' @importFrom lubridate parse_date_time
#' @importFrom magrittr set_colnames
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_if
#' @importFrom dplyr mutate_all
#' @importFrom dplyr slice
#' @importFrom dplyr filter
#' @importFrom dplyr bind_rows
#' @importFrom magrittr "%>%"
#' @title Download San Francisco Beach Water Quality Data
#' @description Downloads and tidies the San Francisco Public Utilities Commission Beach Water Quality Monitoring Program data. Datasets can be downloaded per beach, or the raw data can be downloaded.
#' @rdname san_fran_raw_data
#' @export
san_fran_raw_data <- function(){

  url <- "http://sfwater.org/tasks/lims.csv"

  file <- readr::read_csv(url) %>%
    dplyr::mutate_all(gsub, pattern = "<10", replacement = "0")

  file <-  Filter(function(x) !all(is.na(x)), file)

  return(file)
}

#' @title san_fran_Ocean_Beach
#' @rdname san_fran_Ocean_Beach
#' @description Downloads and tidies the San Francisco Public Utilities Commission Beach Water Quality Monitoring Program data. Datasets can be downloaded per beach, or the raw data can be downloaded.
#' @param location . Choose location.
#' @param status . Show beach status?
#' @export
san_fran_Ocean_Beach <- function(location = c("Sloat Boulevard", "Lincoln Way", "Balboa Street"),
                                 status = FALSE){

  locat <- match.arg(location, choices = c("Sloat Boulevard", "Lincoln Way", "Balboa Street"))

  if(locat == "Sloat Boulevard"){
    loc <- 4602
  } else if(locat == "Lincoln Way"){
    loc <- 4605
  } else{
    loc <- 4604
  }

  url <- paste0("https://sfwater.org/cfapps/LIMS/beachresults3.cfm?loc=", loc)


  if(status == TRUE){
    status_print(url)
  } else{
    result <- scrape_table(url)
    return(result)
  }
}

#' @title san_fran_china_beach
#' @rdname san_fran_china_beach
#' @description Downloads and tidies the San Francisco Public Utilities Commission Beach Water Quality Monitoring Program data. Datasets can be downloaded per beach, or the raw data can be downloaded.
#' @param status . Show beach status?
#' @examples\dontrun{
#' china <- san_fran_china_beach()
#'
#' # See if the beach is open:
#' san_fran_china_beach(status = TRUE)
#' }
#' @export
san_fran_china_beach <- function(status = FALSE){

  url <- "https://sfwater.org/cfapps/LIMS/beachresults3.cfm?loc=4607"

  if(status == TRUE){
    status_print(url)
  } else{
    result <- scrape_table(url)
    return(result)
  }
}

#' @title san_fran_baker_beach
#' @rdname san_fran_baker_beach
#' @description Downloads and tidies the San Francisco Public Utilities Commission Beach Water Quality Monitoring Program data. Datasets can be downloaded per beach, or the raw data can be downloaded.
#' @param location . Choose location.
#' @param status . Show beach status?
#' @export
san_fran_baker_beach <- function(location = c("Baker Beach West", "Baker Beach East",
                                              "Baker Beach at Lobos Creek"),
                                 status = FALSE){

  locat <- match.arg(location, choices = c("Baker Beach West", "Baker Beach East",
                                           "Baker Beach at Lobos Creek"))

  if(locat == "Baker Beach West"){
    loc <- 4608
  } else if(locat == "Baker Beach East"){
    loc <- 4609
  } else{
    loc <- 4610
  }

  url <- paste0("https://sfwater.org/cfapps/LIMS/beachresults3.cfm?loc=", loc)


  if(status == TRUE){
    status_print(url)
  } else{
    result <- scrape_table(url)
    return(result)
  }
}

#' @title san_fran_crissy_field
#' @rdname san_fran_crissy_field
#' @description Downloads and tidies the San Francisco Public Utilities Commission Beach Water Quality Monitoring Program data. Datasets can be downloaded per beach, or the raw data can be downloaded.
#' @param location . Choose location.
#' @param status . Show beach status?
#' @export
san_fran_crissy_field <- function(location = c("West", "East"),
                                  status = FALSE){

  locat <- match.arg(location, choices = c("Baker Beach West", "Baker Beach East",
                                           "Baker Beach at Lobos Creek"))

  if(locat == "West"){
    loc <- 4611
  } else {
    loc <- 4612
  }

  url <- paste0("https://sfwater.org/cfapps/LIMS/beachresults3.cfm?loc=", loc)


  if(status == TRUE){
    status_print(url)
  } else{
    result <- scrape_table(url)
    return(result)
  }
}

#' @title san_fran_aquatic_park
#' @rdname san_fran_aquatic_park
#' @description Downloads and tidies the San Francisco Public Utilities Commission Beach Water Quality Monitoring Program data. Datasets can be downloaded per beach, or the raw data can be downloaded.
#' @param status . Show beach status?
#' @export
san_fran_aquatic_park <- function(status = FALSE){

  url <- "https://sfwater.org/cfapps/LIMS/beachresults3.cfm?loc=4613"

  if(status == TRUE){
    status_print(url)
  } else{
    result <- scrape_table(url)
    return(result)
  }
}

#' @title san_fran_hydeSt_pier
#' @rdname san_fran_hydeSt_pier
#' @description Downloads and tidies the San Francisco Public Utilities Commission Beach Water Quality Monitoring Program data. Datasets can be downloaded per beach, or the raw data can be downloaded.
#' @param status . Show beach status?
#' @export
san_fran_hydeSt_pier <- function(status = FALSE){

  url <- "https://sfwater.org/cfapps/LIMS/beachresults3.cfm?loc=4614"

  if(status == TRUE){
    status_print(url)
  } else{
    result <- scrape_table(url)
    return(result)
  }
}

#' @title san_fran_jackrabbit
#' @rdname san_fran_jackrabbit
#' @description Downloads and tidies the San Francisco Public Utilities Commission Beach Water Quality Monitoring Program data. Datasets can be downloaded per beach, or the raw data can be downloaded.
#' @param status . Show beach status?
#' @export
san_fran_jackrabbit <- function(status = FALSE){
  url <- "https://sfwater.org/cfapps/LIMS/beachresults3.cfm?loc=4615"

  if(status == TRUE){
    status_print(url)
  } else{
    result <- scrape_table(url)
    return(result)
  }
}

#' @title san_fran_windsurfer_circle
#' @rdname san_fran_windsurfer_circle
#' @description Downloads and tidies the San Francisco Public Utilities Commission Beach Water Quality Monitoring Program data. Datasets can be downloaded per beach, or the raw data can be downloaded.
#' @param status . Show beach status?
#' @export
san_fran_windsurfer_circle <- function(status = FALSE){
  url <- "https://sfwater.org/cfapps/LIMS/beachresults3.cfm?loc=4616"

  if(status == TRUE){
    status_print(url)
  } else{
    result <- scrape_table(url)
    return(result)
  }
}

#' @title san_fran_sunnydale_cove
#' @rdname san_fran_sunnydale_cove
#' @description Downloads and tidies the San Francisco Public Utilities Commission Beach Water Quality Monitoring Program data. Datasets can be downloaded per beach, or the raw data can be downloaded.
#' @param status . Show beach status?
#' @export
san_fran_sunnydale_cove <- function(status = FALSE){
  url <- "https://sfwater.org/cfapps/LIMS/beachresults3.cfm?loc=4617"
  if(status == TRUE){
    status_print(url)
  } else{
    result <- scrape_table(url)
    return(result)
  }
}

#' @title san_fran_mission_creek
#' @rdname san_fran_mission_creek
#' @description Downloads and tidies the San Francisco Public Utilities Commission Beach Water Quality Monitoring Program data. Datasets can be downloaded per beach, or the raw data can be downloaded.
#' @param status . Show beach status?
#' @export
san_fran_mission_creek <- function(status = FALSE){
  url <- "https://sfwater.org/cfapps/LIMS/beachresults3.cfm?loc=4618"
  if(status == TRUE){
    status_print(url)
  } else{
    result <- scrape_table(url)
    return(result)
  }
}

#' @title san_fran_islais_creek
#' @rdname san_fran_islais_creek
#' @description Downloads and tidies the San Francisco Public Utilities Commission Beach Water Quality Monitoring Program data. Datasets can be downloaded per beach, or the raw data can be downloaded.
#' @param status . Show beach status?
#' @export
san_fran_islais_creek <- function(status = FALSE){
  url <- "https://sfwater.org/cfapps/LIMS/beachresults3.cfm?loc=4619"
  if(status == TRUE){
    status_print(url)
  } else{
    result <- scrape_table(url)
    return(result)
  }
}


scrape_table <- function(url){
  page <- xml2::read_html(url)

  scrape <- rvest::html_nodes(page, "table") %>% rvest::html_table(fill = TRUE)

  recent <- scrape[[2]] %>% magrittr::set_colnames(c("Date", "Total_Coliform",
                                                     "E_Coli", "Entero_coccus")) %>%
    dplyr::mutate_all(gsub, pattern = "<10", replacement = "0") %>%
    dplyr::slice(3) %>% dplyr::mutate(Date = lubridate::parse_date_time(Date, "mdy!*")) %>%
    dplyr::mutate_if(is.character, as.numeric)

  older <- scrape[[3]] %>% magrittr::set_colnames(c("Date", "Total_Coliform",
                                                    "E_Coli", "Entero_coccus")) %>%
    dplyr::slice(-1:-2) %>%
    dplyr::filter(Date != "Station:") %>%
    dplyr::mutate_all(gsub, pattern = "<10", replacement = "0") %>%
    dplyr::mutate(Date = lubridate::parse_date_time(Date, "mdy!*")) %>%
    dplyr::mutate_if(is.character, as.numeric) %>%
    dplyr::filter(!is.na(Date))

  result <- dplyr::bind_rows(recent, older)
  return(result)
}

status_print <- function(url){
  page <- xml2::read_html(url)
  stat <- rvest::html_nodes(page, "table") %>% rvest::html_table(fill = TRUE) %>% .[[1]] %>%
    .[1,1] %>% gsub("\\r\\n", "", .) %>% print(.)
}
