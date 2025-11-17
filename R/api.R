library(tidyverse)
library(jsonlite)
library(jsonify)
library(leaflet)


#' @title Get API key from environment variable
#'
#' @description
#' Retrieves the API key from the environment variable OKALA_API_KEY.
#' If the variable is not set, an error is raised. # nolint
#'
#' @return The API key as a character string
#'
#' @examples
#' \dontrun{
#'   api_key <- get_key()
#' }
#'
#' @author
#' Adam Varley
#' @export
get_key <- function() {
  api_key <- Sys.getenv("OKALA_API_KEY")
  if (api_key == "") stop("OKALA_API_KEY environment variable not set.")
  return(api_key)
}

#' @title Initiate root URL with API key
#'
#' @description
#' Creates a base URL object that can be used as a root to call endpoints.
#' This requires a project API key, which can be obtained directly from the
#' Okala dashboard.
#'
#' @param api_key A valid API key
#' @param okala_url The base URL for the Okala API (default: production)
#'
#' @return A list containing the root URL and the API key
#'
#' @examples
#' \dontrun{
#'   headers <- auth_headers("your_api_key")
#' }
#'
#' @author
#' Adam Varley
#' @export
auth_headers <- function(api_key,
                         okala_url="https://api.dashboard.okala.io/api/"){
  root <- httr2::request(okala_url)
  d = list(key=api_key,
           root=root)
  return(d)
}

#' @title Initiate root URL with API key (Development)
#'
#' @description
#' Creates a base URL object for the development Okala API.
#' Requires a project API key.
#'
#' @param api_key A valid API key
#' @param okala_url The base URL for the Okala dev API (default: dev)
#'
#' @return A list containing the root URL and the API key
#'
#' @examples
#' \dontrun{
#'   headers <- auth_headers_dev("your_api_key")
#' }
#'
#' @author
#' Adam Varley
#' @export
auth_headers_dev <- function(
    api_key,
    okala_url="https://dev.api.dashboard.okala.io/api/"){
  root <- httr2::request(okala_url)
  d = list(key=api_key,
           root=root)
  return(d)
}


#' @title Get Project Information
#'
#' @description
#' Retrieves information about the active project associated with the
#' provided API key and sets it as the active project.
#'
#' @param hdr A list containing the root URL and API key, as returned by
#'   \link{auth_headers}.
#'
#' @return
#' No return value. Displays a message indicating the active project name.
#'
#' @examples
#' \dontrun{
#'   headers <- auth_headers("your_api_key")
#'   get_project(headers)
#' }
#'
#' @author
#' Adam Varley
#' @export
get_project <- function(hdr){
  urlreq_ap <- httr2::req_url_path_append(hdr$root, "getProject", hdr$key)
  preq <- httr2::req_perform(urlreq_ap)
  resp_str <- httr2::resp_body_json(preq)
  project_name <- resp_str$boundary$features[[1]]$properties$project_name
  message('Setting your active project as - ', project_name)
}


#' @title Get project station metadata
#'
#' @description
#' Retrieve all of the station data associated with your project, including
#' video, audio, image, and eDNA data types.
#'
#' @param hdr A base URL provided and valid API key returned by the
#'   function \link{auth_headers}
#' @param datatype A character vector of data types
#'   c("video","audio","image","eDNA")
#'
#' @return An sf object containing station metadata and geometry
#'
#' @examples
#' \dontrun{
#'   stations <- get_station_info(headers, datatype="video")
#' }
#'
#' @author
#' Adam Varley
#' @export
get_station_info <- function(hdr,
                             datatype=c("video","audio","image","eDNA")){
  urlreq_ap <- httr2::req_url_path_append(
    hdr$root, "getStations", datatype, hdr$key)
  preq <- httr2::req_perform(urlreq_ap)
  resp <- httr2::resp_body_string(preq)
  geojson_response = geojsonsf::geojson_sf(resp)

  return(geojson_response)
}

#' @title Plot stations on a leaflet map
#'
#' @description
#' Plots station locations using leaflet, with circle markers sized by
#' record count.
#'
#' @param geojson_response An sf object containing station metadata and
#'   geometry
#'
#' @return A leaflet map widget
#'
#' @examples
#' \dontrun{
#'   plot_stations(stations)
#' }
#'
#' @author
#' Adam Varley
plot_stations <- function(geojson_response){
    message('Plotting stations')
    leaflet::leaflet(data = geojson_response) %>%
      leaflet::addTiles() %>%
      leaflet::addCircleMarkers(
        lat=sf::st_coordinates(geojson_response)[,2],
        lng=sf::st_coordinates(geojson_response)[,1],
        label = ~paste(device_id),
        popup = ~paste("QR code: ", device_id, "<br>",
        "Start time: ",
        project_system_record_start_timestamp, "<br>",
        "End time: ",
        project_system_record_end_timestamp, "<br>",
        "No. media files: ", record_count, "<br>"
        ),
        color = "red",
        opacity = 0.2,
        stroke = T,
        fillOpacity = 0.6,
        radius = ~ scales::rescale(record_count, c(5,15))
      )

}



#' @title Retrieve media assets for a given project system record ID
#'
#' @description
#' Get all of the station data associated with your project.
#' For data types c("video","audio","image")
#'
#' @param hdr A base URL provided and valid API key returned by the
#'   function \link{auth_headers}
#' @param datatype A character vector of data types
#'   c("video","audio","image","eDNA")
#' @param psrID Unique project system ID for which the media assets
#'   will be retrieved
#'
#' @return A tibble of media assets for the specified project system record
#'
#' @examples
#' \dontrun{
#'   assets <- get_media_assets(headers, datatype="video", psrID=123)
#' }
#'
#' @author
#' Adam Varley
#' @export
get_media_assets <- function(hdr,
                             datatype=c("video","audio","image","eDNA"),
                             psrID){

  urlreq_ap <- httr2::req_url_path_append(
    hdr$root, "getMediaAssets", datatype, hdr$key) %>%
    httr2::req_method("POST") %>%
    httr2::req_body_json(data=psrID)

  preq <- httr2::req_perform(urlreq_ap,verbosity=3)
  resp <- httr2::resp_body_string(preq)

  return(jsonlite::fromJSON(resp) %>% tibble::as_tibble())

}



#' @title Get project labels for either bioacoustics or camera
#'
#' @description
#' Labels are derived by using either suggested labels on the platform or
#' by manually adding labels from the wider database.
#'
#' @param hdr A base URL provided and valid API key returned by the
#'   function \link{auth_headers}
#' @param labeltype A character vector specifying the label type
#'   ('Bioacoustic' or 'Camera')
#'
#' @return A tibble containing project labels
#'
#' @examples
#' \dontrun{
#'   labels <- get_project_labels(headers, labeltype='Camera')
#' }
#'
#' @author
#' Adam Varley
#' @export
get_project_labels <- function(hdr,
                               labeltype = c('Bioacoustic','Camera')){
  urlreq_ap <- httr2::req_url_path_append(
    hdr$root, "getProjectLabels", labeltype, hdr$key)
  preq <- httr2::req_perform(urlreq_ap)
  resp <- httr2::resp_body_string(preq)

  return(jsonlite::fromJSON(resp) %>% tibble::as_tibble())
}

#' @title Add project labels for either bioacoustics or camera
#'
#' @description
#' Add labels so labellers have access to them in the Dashboard. Labels are
#' derived by using either suggested labels on the platform or by manually
#' adding labels from the wider database.
#'
#' @param hdr A base URL provided and valid API key returned by the
#'   function \link{auth_headers}
#' @param labeltype A character vector specifying the label type
#'   ('Bioacoustic' or 'Camera')
#' @param labels A label object list specifying the labels to be added
#'
#' @return A success message as a list
#'
#' @examples
#' \dontrun{
#'   add_project_labels(headers, labeltype='Camera', labels=my_labels)
#' }
#'
#' @author
#' Adam Varley
#' @export
add_project_labels <- function(hdr,
                               labeltype = c('Bioacoustic','Camera'),labels){
  urlreq_ap <- httr2::req_url_path_append(hdr$root,"addProjectLabels",labeltype,hdr$key)
  urlreq_ap <- urlreq_ap |>  httr2::req_method("POST") |> httr2::req_body_json(data=labels)
  preq <- httr2::req_perform(urlreq_ap)
  resp <- httr2::resp_body_json(preq)

  return(resp)
}

# Utility function to replace NULLs with NAs in a data frame
replace_nas <- function(df){
  df[sapply(df,function(x) is.null(x))] = NA
  return(df)
}

#' @title Get labels from the wider IUCN database (all species)
#'
#' @description
#' Retrieve labels from the wider IUCN database, optionally filtered by a search term.
#'
#' @param hdr A base URL provided and valid API key returned by the function \link{auth_headers}
#' @param offset An integer specifying the offset for the query
#' @param limit An integer specifying the limit for the query
#' @param search_term A character vector specifying the search term to be used (optional)
#'
#' @return A list containing tabular data and pagination information for iterative calls
#'
#' @examples
#' \dontrun{
#'   getIUCNLabels(headers, offset=0, limit=100, search_term="horse")
#' }
#'
#' @author
#' Adam Varley
#' @export
getIUCNLabels <- function(hdr, offset, limit,search_term=NULL){
  if (is.null(search_term)){
    search_term = ""
  }
  if (limit > 20000){
    stop("Limit cannot be greater than 20000")
  }
  urlreq_ap <- httr2::req_url_path_append(hdr$root,"getIUCNLabels",hdr$key)
  urlreq_ap <- urlreq_ap |>  httr2::req_method("GET") |> httr2::req_url_query("offset" = offset, "limit" = limit,'search_term'= search_term)

  preq <- httr2::req_perform(urlreq_ap)
  resp <- httr2::resp_body_json(preq)
  resp_table <- lapply(resp$table, function(x) x %>% replace_nas() %>%  tibble::as_tibble()) %>% dplyr::bind_rows()

  return (list(data=resp_table, total=resp$pagination_state$total, offset=resp$pagination_state$offset, limit=resp$pagination_state$limit))
}


#' @title Add labels from the wider IUCN database (all species)
#'
#' @description
#' Add labels from the wider IUCN database in chunks.
#'
#' @param hdr A base URL provided and valid API key returned by the function \link{auth_headers}
#' @param labels A data frame of labels to add
#' @param chunksize An integer specifying the chunk size for the submission
#'
#' @return A success message as a list
#'
#' @examples
#' \dontrun{
#'   add_IUCN_labels(headers, labels=my_labels, chunksize=200)
#' }
#'
#' @author
#' Adam Varley
#' @export
add_IUCN_labels <- function(hdr,labels,chunksize){

  if(nrow(labels) < 100){
    message('Data is too small to chunk, submitting all data')
    chunksize = nrow(labels)
    spl.dt = list(labels)
  } else {

    if(chunksize > nrow(labels)){
      message('chunksize is bigger than length of data altering chunkszie to ', nrow(labels))
      chunksize = nrow(labels)/2
    } else {
      spl.dt <- split( labels , cut(seq_len(nrow(labels)), round(nrow(labels)/chunksize)))

    }
  }


  i = 31
  for (i in seq_along(spl.dt)){

    urlreq_ap <- httr2::req_url_path_append(hdr$root,"addIUCNLabels",hdr$key)
    urlreq_ap <- urlreq_ap |>  httr2::req_method("POST") |> httr2::req_body_json(data=spl.dt[[i]])

    preq <- httr2::req_perform(urlreq_ap,verbosity=3)
    resp <- httr2::resp_body_json(preq)

    message('submitted ',i*chunksize,' labels of ', nrow(nrow(labels)))
  }

  return(resp)
}


# Internally used function to send updated labels in chunks

sendupatedlabels <- function(hdr,datachunk) {


  datachunk = jsonlite::toJSON(datachunk,pretty=TRUE)

  urlreq_ap <- httr2::req_url_path_append(hdr$root,"updateSegmentLabels", hdr$key)
  urlreq_ap <- urlreq_ap |>  httr2::req_method("PUT")  |> httr2::req_body_json(jsonlite::fromJSON(datachunk))
  #
  preq <- httr2::req_perform(urlreq_ap,verbosity=3)
  resp <- httr2::resp_body_string(preq)

  return(jsonlite::fromJSON(resp))
}

#' @title Push new labels using a chunked process
#'
#' @description
#' Push new labels to the platform in chunks.
#'
#' @param hdr A base URL provided and valid API key returned by the function \link{auth_headers}
#' @param submission_records A tibble containing the records to be submitted
#' @param chunksize An integer specifying the chunk size for the submission
#'
#' @return A list containing tabular data and pagination information for iterative calls
#'
#' @examples
#' \dontrun{
#'   push_new_labels(headers, submission_records, chunksize=30)
#' }
#'
#' @author
#' Adam Varley
#' @export
push_new_labels <- function(hdr,submission_records,chunksize){

  if(chunksize > nrow(submission_records)){
    message('chunksize is bigger than length of data altering chunkszie to ', nrow(labels))
    chunksize = nrow(submission_records)
  }

  spl.dt <- split( submission_records , cut(seq_len(nrow(submission_records)), round(nrow(submission_records)/chunksize)))
  spl.dt[1]
  i=1
  for (i in seq_along(spl.dt)){

    sendupatedlabels(hdr,datachunk=spl.dt[[i]])
    message('submitted ',i*chunksize,' labels of ', nrow(submission_records))
  }
}



# Internal function used to chunk media metadata
send_media_chunks <- function(hdr, datachunk) {
  datachunk = jsonlite::toJSON(datachunk,pretty=TRUE)

  urlreq_ap <- httr2::req_url_path_append(hdr$root,"updateTimestamps", hdr$key)
  urlreq_ap <- urlreq_ap |>  httr2::req_method("PUT")  |> httr2::req_body_json(jsonlite::fromJSON(datachunk))
  #
  preq <- httr2::req_perform(urlreq_ap,verbosity=3)
  resp <- httr2::resp_body_string(preq)

  return(jsonlite::fromJSON(resp))
}


#' @title Push new timestamps to the platform in chunks
#'
#' @description
#' Push new timestamps to the platform in chunks.
#'
#' @param hdr A base URL provided and valid API key returned by the function \link{auth_headers}
#' @param media_metadata A tibble containing the media metadata to be submitted
#' @param chunksize An integer specifying the chunk size for the submission
#'
#' @return A success message as a list
#'
#' @examples
#' \dontrun{
#'   push_new_timestamps(headers, media_metadata, chunksize=100)
#' }
#'
#' @author
#' Adam Varley
#' @export
push_new_timestamps <- function(hdr, media_metadata, chunksize) {
    if(chunksize > nrow(media_metadata)){
        message('chunksize is bigger than length of data altering chunkszie to ', nrow(media_metadata))
        chunksize = nrow(media_metadata)
    }

    spl.dt <- split( media_metadata , cut(seq_len(nrow(media_metadata)), round(nrow(media_metadata)/chunksize)))
    i=1
    for (i in seq_along(spl.dt)){

        send_media_chunks(hdr,spl.dt[[i]])
        message('submitted ',i*chunksize,' timestamps of ', nrow(media_metadata))
    }
}

#' @title Set blank status for segment labels
#'
#' @description
#' Marks or unmarks segment labels as blank for a given list of segment record IDs.
#'
#' @param hdr A base URL provided and valid API key returned by the function \link{auth_headers}.
#' @param blank_status A boolean value indicating whether to mark as blank (TRUE) or unblank (FALSE).
#' @param segment_record_ids A numeric vector of segment record IDs to update.
#'
#' @return A list containing the API response message.
#'
#' @examples
#' \dontrun{
#'   # Mark segments as blank
#'   set_segment_blank_status(headers, blank_status = TRUE, segment_record_ids = c(101, 102, 103))
#'   # Unmark segments as blank
#'   set_segment_blank_status(headers, blank_status = FALSE, segment_record_ids = c(101, 102, 103))
#' }
#'
#' @author
#' Adam Varley
#' @export
set_segment_blank_status <- function(hdr, blank_status, segment_record_ids) {
  status_str <- tolower(as.character(blank_status))

  urlreq_ap <- httr2::req_url_path_append(hdr$root, "segmentLabelsBlankStatus", hdr$key, status_str) %>%
    httr2::req_method("PUT") %>%
    httr2::req_body_json(data = segment_record_ids)

  preq <- httr2::req_perform(urlreq_ap)
  resp <- httr2::resp_body_json(preq)

  message(resp$message)
  return(resp)
}

#' @title Check eDNA Labels
#'
#' @description
#' Validates eDNA records against the Okala database and returns matching labels.
#' Uses a hierarchical taxonomy approach: species -> genus -> family -> order -> class -> phylum -> kingdom.
#' Returns the most specific taxonomic level that matches the database.
#'
#' @param hdr A list containing the root URL and API key for authentication (from auth_headers()).
#' @param edna_data A data frame or tibble with eDNA records containing the following columns:
#'   \itemize{
#'     \item marker_name: The genetic marker used (required)
#'     \item sequence: DNA sequence (required)
#'     \item primer: Primer used for amplification (required)
#'     \item timestamp: Timestamp for the record (required)
#'     \item kingdom: Kingdom taxonomic rank (optional)
#'     \item phylum: Phylum taxonomic rank (optional)
#'     \item class: Class taxonomic rank (optional, note: 'class_' can also be used)
#'     \item order: Order taxonomic rank (optional)
#'     \item family: Family taxonomic rank (optional)
#'     \item genus: Genus taxonomic rank (optional)
#'     \item species: Species taxonomic rank (optional)
#'     \item confidence: Confidence score 0-100 (optional, defaults to 100)
#'   }
#'
#' @return A tibble with the original data plus additional columns:
#'   \itemize{
#'     \item label: The matched label name from the database
#'     \item label_id: The database ID of the matched label
#'     \item status: "success" or "error"
#'     \item message: Status message describing the result
#'   }
#'
#' @examples
#' \dontrun{
#'   headers <- auth_headers()
#'   
#'   edna_records <- data.frame(
#'     marker_name = "COI",
#'     sequence = "ACGTACGT",
#'     primer = "mlCOIintF",
#'     timestamp = "2024-01-15 10:30:00",
#'     species = "Panthera leo",
#'     genus = "Panthera",
#'     family = "Felidae",
#'     confidence = 95
#'   )
#'   
#'   validated <- check_edna_labels(headers, edna_records)
#' }
#'
#' @author
#' Adam Varley
#' @export
check_edna_labels <- function(hdr, edna_data) {
  # Validate required columns
  required_cols <- c("marker_name", "sequence", "primer", "timestamp")
  missing_cols <- setdiff(required_cols, names(edna_data))
  
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Ensure confidence column exists with default value of 100
  if (!"confidence" %in% names(edna_data)) {
    edna_data$confidence <- 100
  }
  
  # Handle class_ vs class column naming
  if ("class_" %in% names(edna_data) && !"class" %in% names(edna_data)) {
    edna_data$class <- edna_data$class_
  }
  
  # Convert to list format for JSON
  edna_list <- lapply(seq_len(nrow(edna_data)), function(i) {
    row <- as.list(edna_data[i, ])
    # Remove NA values to send cleaner JSON
    row[!is.na(row)]
  })
  
  # Make API request
  urlreq_ap <- httr2::req_url_path_append(hdr$root, "checkeDNALabels", hdr$key) %>%
    httr2::req_method("POST") %>%
    httr2::req_body_json(data = edna_list)
  
  preq <- httr2::req_perform(urlreq_ap)
  resp <- httr2::resp_body_json(preq, simplifyVector = TRUE)
  
  # Convert to tibble
  result <- tibble::as_tibble(resp)
  
  message("Validated ", nrow(result), " eDNA records")
  return(result)
}

#' @title Upload eDNA Records
#'
#' @description
#' Uploads validated eDNA records to a specific project system record.
#' Only records with status='success' from check_edna_labels will be uploaded.
#'
#' @param hdr A list containing the root URL and API key for authentication
#'   (from auth_headers()).
#' @param validated_data A data frame or tibble containing validated eDNA
#'   records from check_edna_labels. Must include all original columns plus
#'   label, label_id, status, and message fields.
#' @param project_system_record_id The project system record ID to which the
#'   eDNA records will be uploaded.
#'
#' @return A tibble with the upload response for each record
#'
#' @examples
#' \dontrun{
#'   headers <- auth_headers()
#'   
#'   # First validate the records
#'   validated <- check_edna_labels(headers, edna_records)
#'   
#'   # Then upload only successful validations
#'   upload_result <- upload_edna_records(
#'     headers,
#'     validated,
#'     project_system_record_id = 123
#'   )
#' }
#'
#' @author
#' Adam Varley
#' @export
upload_edna_records <- function(hdr, validated_data,
                                 project_system_record_id) {
  # Filter for only successful records
  if (!"status" %in% names(validated_data)) {
    stop("Data must be validated first using check_edna_labels()")
  }
  
  successful_records <- validated_data[validated_data$status == "success", ]
  
  if (nrow(successful_records) == 0) {
    stop("No successful records to upload. All records failed validation.")
  }
  
  message("Uploading ", nrow(successful_records), " validated eDNA records")
  
  # Convert to list format for JSON
  edna_list <- lapply(seq_len(nrow(successful_records)), function(i) {
    row <- as.list(successful_records[i, ])
    # Remove NA values to send cleaner JSON
    row[!is.na(row)]
  })
  
  # Make API request
  urlreq_ap <- httr2::req_url_path_append(
    hdr$root, "uploadeDNA", hdr$key,
    as.character(project_system_record_id)) %>%
    httr2::req_method("POST") %>%
    httr2::req_body_json(data = edna_list)
  
  preq <- httr2::req_perform(urlreq_ap)
  resp <- httr2::resp_body_json(preq, simplifyVector = TRUE)
  
  # Convert to tibble
  result <- tibble::as_tibble(resp)
  
  message("Upload complete: ", nrow(result), " records processed")
  return(result)
}

#' @title Push Phone Observations
#'
#' @description
#' Uploads field observations from mobile devices to the Okala platform.
#' Supports uploading features with observations and optional media files
#' (photos, videos, audio).
#'
#' @param hdr A list containing the root URL and API key for authentication
#'   (from auth_headers()).
#' @param project_id The project ID to which observations will be uploaded.
#' @param feature_payload A list of feature records. Each feature should have:
#'   \itemize{
#'     \item feature_uuid: UUID for the feature (character)
#'     \item project_system_id: Project system ID (integer)
#'     \item procedure_id: Procedure ID (integer)
#'     \item procedure_start_timestamp: Start timestamp (POSIXct or character)
#'     \item procedure_end_timestamp: End timestamp (POSIXct or character)
#'     \item created_by_method: Method used (e.g., "drawn", "gps")
#'     \item geometry: GeoJSON geometry object (list with type and coordinates)
#'     \item observations: List of observation features (see details)
#'   }
#' @param device_settings A list containing device metadata:
#'   \itemize{
#'     \item battery_level: Battery percentage (0-100)
#'     \item carrier: Mobile carrier name
#'     \item build_number: App build number
#'     \item build_id: App build ID
#'     \item device_id: Unique device identifier
#'     \item phone_model: Device model
#'     \item phone_operating_system: OS version
#'     \item device_created_at: Device creation timestamp
#'     \item device_last_used: Last usage timestamp
#'   }
#' @param media_files Optional list of file paths to upload. Files are matched
#'   by filename in observation data arrays.
#'
#' @return A tibble with upload response messages
#'
#' @details
#' Each observation in feature_payload should be a GeoJSON Feature with:
#' \itemize{
#'   \item type: "Feature"
#'   \item geometry: Point geometry with coordinates
#'   \item properties: List with item_uuid, item_type, observation_uuid,
#'     observation_created_at, and data (array of values/filenames)
#' }
#'
#' For media observations (phone-photo, phone-video, phone-audio), the data
#' array should contain filenames that match the media_files parameter.
#'
#' @examples
#' \dontrun{
#'   headers <- auth_headers()
#'   
#'   # Define feature with observations
#'   feature <- list(
#'     feature_uuid = uuid::UUIDgenerate(),
#'     project_system_id = 123,
#'     procedure_id = 456,
#'     procedure_start_timestamp = Sys.time(),
#'     procedure_end_timestamp = Sys.time(),
#'     created_by_method = "drawn",
#'     geometry = list(
#'       type = "Polygon",
#'       coordinates = list(list(
#'         c(-0.1, 51.5),
#'         c(-0.1, 51.6),
#'         c(0.0, 51.6),
#'         c(0.0, 51.5),
#'         c(-0.1, 51.5)
#'       ))
#'     ),
#'     observations = list(
#'       list(
#'         type = "Feature",
#'         geometry = list(type = "Point", coordinates = c(-0.05, 51.55)),
#'         properties = list(
#'           item_uuid = uuid::UUIDgenerate(),
#'           item_type = "text",
#'           observation_uuid = uuid::UUIDgenerate(),
#'           observation_created_at = Sys.time(),
#'           data = list("Sample observation")
#'         )
#'       )
#'     )
#'   )
#'   
#'   device <- list(
#'     battery_level = 85,
#'     carrier = "Vodafone",
#'     build_number = "1.0.0",
#'     build_id = "build-123",
#'     device_id = "device-xyz",
#'     phone_model = "iPhone 14",
#'     phone_operating_system = "iOS 17",
#'     device_created_at = Sys.time(),
#'     device_last_used = Sys.time()
#'   )
#'   
#'   result <- push_phone_observations(
#'     headers,
#'     project_id = 1,
#'     feature_payload = list(feature),
#'     device_settings = device
#'   )
#' }
#'
#' @author
#' Adam Varley
#' @export
push_phone_observations <- function(hdr, project_id, feature_payload,
                                    device_settings, media_files = NULL) {
  
  # Build the request body
  request_body <- list(
    feature_payload = feature_payload,
    device_settings = device_settings
  )
  
  # Convert to JSON
  json_body <- jsonlite::toJSON(request_body, auto_unbox = TRUE)
  
  # Build the request
  urlreq_ap <- httr2::req_url_path_append(
    hdr$root,
    "pushPhoneObservations",
    hdr$key,
    as.character(project_id)
  ) %>%
    httr2::req_method("POST")
  
  # If media files are provided, use multipart form
  if (!is.null(media_files)) {
    message("Uploading ", length(media_files), " media file(s)")
    
    # Build multipart form parts
    form_parts <- list(
      device_upload = httr2::curl_form_data(
        json_body,
        type = "application/json"
      )
    )
    
    # Add each file as a separate part
    for (file_path in media_files) {
      if (!file.exists(file_path)) {
        stop("File not found: ", file_path)
      }
      
      file_name <- basename(file_path)
      # Determine MIME type based on extension
      ext <- tolower(tools::file_ext(file_path))
      mime_type <- switch(ext,
        jpg = "image/jpeg",
        jpeg = "image/jpeg",
        png = "image/png",
        mp4 = "video/mp4",
        mov = "video/quicktime",
        mp3 = "audio/mpeg",
        m4a = "audio/mp4",
        wav = "audio/wav",
        "application/octet-stream"
      )
      
      form_parts[[file_name]] <- httr2::curl_form_file(
        file_path,
        type = mime_type
      )
    }
    
    urlreq_ap <- urlreq_ap %>%
      httr2::req_body_multipart(!!!form_parts)
  } else {
    # No media files, just send JSON
    urlreq_ap <- urlreq_ap %>%
      httr2::req_body_json(data = request_body)
  }
  
  # Perform request
  preq <- httr2::req_perform(urlreq_ap)
  resp <- httr2::resp_body_json(preq, simplifyVector = TRUE)
  
  # Convert response to tibble
  if ("messages" %in% names(resp)) {
    result <- tibble::as_tibble(resp$messages)
    
    # Count successes and errors
    n_success <- sum(result$response_type == "success")
    n_error <- sum(result$response_type == "error")
    n_warning <- sum(result$response_type == "warning")
    
    message("Upload complete: ", n_success, " success, ",
            n_error, " errors, ", n_warning, " warnings")
  } else {
    result <- tibble::as_tibble(resp)
    message("Upload complete")
  }
  
  return(result)
}

#' @title Build Observation Payload from Dataframe
#'
#' @description
#' Helper function to convert flat dataframes into the nested structure
#' required by push_phone_observations. Makes it easier to work with
#' observations in R's tabular format.
#'
#' @param observations_df A dataframe with one row per observation containing:
#'   \itemize{
#'     \item feature_uuid: UUID grouping observations into features
#'     \item observation_uuid: Unique ID for this observation
#'     \item observation_created_at: Timestamp when observation was recorded
#'     \item item_uuid: ID of the item/question being answered
#'     \item item_type: Type of observation (text, numeric, choice, label,
#'       phone-photo, phone-video, phone-audio)
#'     \item observation_value: The value(s) - for media, use filename
#'     \item longitude: Observation point longitude
#'     \item latitude: Observation point latitude
#'   }
#' @param features_df A dataframe with one row per feature containing:
#'   \itemize{
#'     \item feature_uuid: UUID for the feature
#'     \item project_system_id: Project system ID
#'     \item procedure_id: Procedure ID
#'     \item procedure_start_timestamp: Start time
#'     \item procedure_end_timestamp: End time
#'     \item created_by_method: Method (e.g., "drawn", "gps")
#'     \item geometry_type: "Point", "Polygon", etc.
#'     \item geometry_coords: JSON string or list of coordinates
#'   }
#'
#' @return A list structure ready for push_phone_observations
#'
#' @examples
#' \dontrun{
#'   # Create observations dataframe
#'   obs_df <- data.frame(
#'     feature_uuid = rep("feature-1", 2),
#'     observation_uuid = c("obs-1", "obs-2"),
#'     observation_created_at = Sys.time(),
#'     item_uuid = c("item-1", "item-2"),
#'     item_type = c("text", "numeric"),
#'     observation_value = c("Tree", "15"),
#'     longitude = c(-0.05, -0.05),
#'     latitude = c(51.55, 51.55)
#'   )
#'   
#'   # Create features dataframe
#'   feat_df <- data.frame(
#'     feature_uuid = "feature-1",
#'     project_system_id = 123,
#'     procedure_id = 456,
#'     procedure_start_timestamp = Sys.time(),
#'     procedure_end_timestamp = Sys.time(),
#'     created_by_method = "drawn",
#'     geometry_type = "Polygon",
#'     geometry_coords = '[[[−0.1,51.5],[−0.1,51.6],[0,51.6],[0,51.5],[−0.1,51.5]]]'
#'   )
#'   
#'   payload <- build_observation_payload(obs_df, feat_df)
#' }
#'
#' @author
#' Adam Varley
#' @export
build_observation_payload <- function(observations_df, features_df) {
  
  # Validate required columns
  obs_required <- c("feature_uuid", "observation_uuid", "observation_created_at",
                    "item_uuid", "item_type", "observation_value",
                    "longitude", "latitude")
  feat_required <- c("feature_uuid", "project_system_id", "procedure_id",
                     "procedure_start_timestamp", "procedure_end_timestamp",
                     "created_by_method", "geometry_type", "geometry_coords")
  
  missing_obs <- setdiff(obs_required, names(observations_df))
  missing_feat <- setdiff(feat_required, names(features_df))
  
  if (length(missing_obs) > 0) {
    stop("Missing required observation columns: ",
         paste(missing_obs, collapse = ", "))
  }
  
  if (length(missing_feat) > 0) {
    stop("Missing required feature columns: ",
         paste(missing_feat, collapse = ", "))
  }
  
  # Split observations by feature
  obs_by_feature <- split(observations_df,
                          observations_df$feature_uuid)
  
  # Build feature list
  feature_list <- lapply(seq_len(nrow(features_df)), function(i) {
    feat <- features_df[i, ]
    
    # Parse geometry coordinates
    if (is.character(feat$geometry_coords)) {
      geom_coords <- jsonlite::fromJSON(feat$geometry_coords)
    } else {
      geom_coords <- feat$geometry_coords
    }
    
    # Get observations for this feature
    feature_obs <- obs_by_feature[[as.character(feat$feature_uuid)]]
    
    # Build observation list
    observations_list <- list()
    if (!is.null(feature_obs) && nrow(feature_obs) > 0) {
      observations_list <- lapply(seq_len(nrow(feature_obs)), function(j) {
        obs <- feature_obs[j, ]
        
        # Parse observation value - could be single value or array
        obs_data <- if (grepl(",", obs$observation_value)) {
          strsplit(as.character(obs$observation_value), ",")[[1]]
        } else {
          list(obs$observation_value)
        }
        
        # Convert numeric values
        if (obs$item_type == "numeric") {
          obs_data <- lapply(obs_data, as.numeric)
        } else if (obs$item_type == "label") {
          obs_data <- lapply(obs_data, as.integer)
        }
        
        list(
          type = "Feature",
          geometry = list(
            type = "Point",
            coordinates = c(obs$longitude, obs$latitude)
          ),
          properties = list(
            item_uuid = obs$item_uuid,
            item_type = obs$item_type,
            observation_uuid = obs$observation_uuid,
            observation_created_at = format(obs$observation_created_at,
                                           "%Y-%m-%dT%H:%M:%S"),
            data = obs_data
          )
        )
      })
    }
    
    list(
      feature_uuid = feat$feature_uuid,
      project_system_id = as.integer(feat$project_system_id),
      procedure_id = as.integer(feat$procedure_id),
      procedure_start_timestamp = format(feat$procedure_start_timestamp,
                                        "%Y-%m-%dT%H:%M:%SZ"),
      procedure_end_timestamp = format(feat$procedure_end_timestamp,
                                      "%Y-%m-%dT%H:%M:%SZ"),
      created_by_method = feat$created_by_method,
      geometry = list(
        type = feat$geometry_type,
        coordinates = geom_coords
      ),
      observations = observations_list
    )
  })
  
  return(feature_list)
}

#' @title Build Device Settings from Dataframe
#'
#' @description
#' Helper function to convert a single-row dataframe or named list into
#' the device_settings structure required by push_phone_observations.
#'
#' @param device_df A dataframe or list with device information:
#'   \itemize{
#'     \item battery_level: Battery percentage (0-100)
#'     \item carrier: Mobile carrier name
#'     \item build_number: App build number
#'     \item build_id: App build ID
#'     \item device_id: Unique device identifier
#'     \item phone_model: Device model
#'     \item phone_operating_system: OS version
#'     \item device_created_at: Device creation timestamp (optional)
#'     \item device_last_used: Last usage timestamp (optional)
#'   }
#'
#' @return A list structure ready for push_phone_observations
#'
#' @examples
#' \dontrun{
#'   device_df <- data.frame(
#'     battery_level = 85,
#'     carrier = "Vodafone",
#'     build_number = "1.0.0",
#'     build_id = "build-123",
#'     device_id = "device-xyz",
#'     phone_model = "iPhone 14",
#'     phone_operating_system = "iOS 17"
#'   )
#'   
#'   device_settings <- build_device_settings(device_df)
#' }
#'
#' @author
#' Adam Varley
#' @export
build_device_settings <- function(device_df) {
  
  required <- c("battery_level", "carrier", "build_number", "build_id",
                "device_id", "phone_model", "phone_operating_system")
  
  # Convert to list if dataframe
  if (is.data.frame(device_df)) {
    device_df <- as.list(device_df[1, ])
  }
  
  missing <- setdiff(required, names(device_df))
  if (length(missing) > 0) {
    stop("Missing required device fields: ", paste(missing, collapse = ", "))
  }
  
  # Build device settings with defaults
  settings <- list(
    battery_level = as.integer(device_df$battery_level),
    carrier = as.character(device_df$carrier),
    build_number = as.character(device_df$build_number),
    build_id = as.character(device_df$build_id),
    device_id = as.character(device_df$device_id),
    phone_model = as.character(device_df$phone_model),
    phone_operating_system = as.character(device_df$phone_operating_system),
    device_created_at = if (!is.null(device_df$device_created_at)) {
      format(device_df$device_created_at, "%Y-%m-%dT%H:%M:%S")
    } else {
      format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
    },
    device_last_used = if (!is.null(device_df$device_last_used)) {
      format(device_df$device_last_used, "%Y-%m-%dT%H:%M:%SZ")
    } else {
      format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
    }
  )
  
  return(settings)
}




