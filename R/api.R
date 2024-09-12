library(tidyverse)
#okala_URL <- "https://dev.api.dashboard.okala.io/api/"
# api_key <- "D2lfE2pxnrWI83daSqYqPcZDDSpwEIGT4lgNrOtv7ML5Qkk7qORwBmgvg7e46wd5MTuaVRwAMzaDuycrfH6Wuxy1Ti0PSFnHFeIF"
api_key <- "OIqeRL4QD2IDFVmiW90mbUoU3wTLZRXCyAeVvo8UIagOM17CXWnu8ajuz09CVcwrTfxArhFgULH3pRZvNDsBgpb3TIb5Lxi4FjH3"

#' Initiate root URL with API key
#'
#' Creates a base URL object that can be used as a root to call endpoints.This requires a project API key, which can be obtained directly from the Okala dashboard.
#'
#' @param api_key A valid API key
#'
#' @return This function returns a list containing the root URL and the API key
#'
#' @export
auth_headers <- function(api_key){
  okala_url <- "http://localhost:8000/api/"
  # okala_url <- "https://dev.api.dashboard.okala.io/api/"
  root <- httr2::request(okala_url)
  d = list(key=api_key,
           root=root)
  return(d)
}

headers <- auth_headers(api_key)

get_project <- function(hdr=headers){
  urlreq_ap <- httr2::req_url_path_append(hdr$root,"getProject",hdr$key)
  preq <- httr2::req_perform(urlreq_ap)
  resp <- httr2::resp_body_json(preq)
  resp <- jsonlite::toJSON(resp$boundary)
  geojson_response <-  geojsonsf::geojson_sf(resp)
  return(geojson_response)
}

project_details = get_project(headers)

#' Get project station metadata
#'
#' Retrieve all of the station data associated with your project, including video, audio, image, and eDNA data types.
#'
#' @param hdr A base URL provided and valid API key returned by the function \link{auth_headers}
#' @param datatype A character vector of data types c("video","audio","image","eDNA")
#'
#' @return This function returns an sf object containing station metadata and geometry
#' @export
get_station_info <- function(hdr,
                             datatype=c("video","audio","image","eDNA")){
  urlreq_ap <- httr2::req_url_path_append(hdr$root,"getStations",datatype,hdr$key)
  preq <- httr2::req_perform(urlreq_ap)
  resp <- httr2::resp_body_string(preq)
  geojson_response = geojsonsf::geojson_sf(resp)
  return(geojson_response)
}

stations <- get_station_info(hdr=headers,datatype="video")

#' Retrieve media assets for a given project system record ID
#'
#' Get all of the station data associated with your project. For data types c("video","audio","image")
#'
#' @param hdr A base URL provided and valid API key returned by the function \link{auth_headers}
#' @param datatype A character vector of data types c("video","audio","image","eDNA")
#' @param psrID Unique project system ID for which the media assets will be retrieved
#'
#' @return This function returns a tibble of media assets for the specified project system record
#'
#' @export
get_media_assets <- function(hdr,
                             datatype=c("video","audio","image","eDNA"),
                             psrID){

  urlreq_ap <- httr2::req_url_path_append(hdr$root,"getMediaAssets",datatype,psrID,hdr$key)
  preq <- httr2::req_perform(urlreq_ap)
  resp <- httr2::resp_body_string(preq)

  return(jsonlite::fromJSON(resp) %>% tibble::as_tibble())

}

media_labels <- get_media_assets(hdr=headers,
                                 datatype="video",
                                 psrID=stations$project_system_record_id[5])

#' Get project labels for either bioacoustics or camera
#'
#' Labels are derived by using either suggested labels on the platform or by manually
#' adding labels from the wider database
#'
#' @param hdr A base URL provided and valid API key returned by the function \link{auth_headers}
#' @param labeltype A character vector specifying the label type ('Bioacoustic' or 'Camera')
#'
#' @return This function returns a tibble containing project labels
#'
#' @export
get_project_labels <- function(hdr,
                               labeltype = c('Bioacoustic','Camera')){
  urlreq_ap <- httr2::req_url_path_append(hdr$root,"getProjectLabels",labeltype,hdr$key)
  preq <- httr2::req_perform(urlreq_ap)
  resp <- httr2::resp_body_string(preq)

  return(jsonlite::fromJSON(resp) %>% tibble::as_tibble())
}

project_camera_labels <- get_project_labels(hdr=headers,labeltype='Camera')

### Get data frame of updated label IDs

new_labels <- function(Hdr,
                       Data,
                       ProjectLabels,
                       PSRIs,
                       Datatype=c("video","audio","image","eDNA")){

  RES <- list()

  for (psri in 1:length(PSRIs)){

    ml <- get_media_assets(hdr=Hdr,
                           datatype=Datatype,
                           psrID=PSRIs[psri])

    ml$label_f <- NA
    ml$label_fspp <- NA

    for (i in 1:nrow(ml)){
      lab <- ml$species[i]
      mfrl <- ml$media_file_reference_location[i]
      ss <- Data[Data$new_vid_id==mfrl,]
      if (nrow(ss)==0){  # If there are no records in the correctly labelled dataset for that specific camera location
        ml$label_f[i] <- NA
        ml$label_fspp[i] <- NA
      }
      else{
        nr <- nrow(ss)
        for (j in 1:nr){
          sss <- ss[j,]
          blank <- ifelse(sss$species_label=="Blank",1,0)
          if (blank==1){   # If correct label is blank
            ml$label_f[i] <- NA
            ml$label_fspp[i] <- -1
          }
          else{
            lab_correct <- sss$latin_name[1]
            lab_correct2 <- strsplit(lab_correct, split="_")
            lab_correct3 <- paste(c(lab_correct2[[1]][1],lab_correct2[[1]][2]),collapse=" ")
            temp <- ProjectLabels$label_id[which(ProjectLabels$label==lab_correct3)]
            if (length(temp)>0){
              ml$label_f[i] <- temp
              ml$label_fspp[i] <- lab_correct3
            }
            else{
              ml$label_f[i] <- NA
            }
          }
        }
      }
    }

    RES[[psri]] <- ml

    print(psri)
  }

  return(do.call(rbind,RES))

}

DATA <- read.csv("P0032_species_ID_list_FINAL.csv")
PCLs <- get_project_labels(hdr=headers,labeltype='Camera')
PBLs <- get_project_labels(hdr=headers,labeltype='Bioacoustic')
PCL <- rbind(PCLs,PBLs)
uPSRI <- unique(stations$project_system_record_id)

testt <- new_labels(Hdr=headers,
                    Data=DATA,
                    ProjectLabels=PCL,
                    PSRIs=uPSRI,
                    Datatype="video")


testt$label_f = ifelse(is.na(testt$label_f),-1,testt$label_f)

testt$label_fspp = ifelse(testt$label_fspp==-1,'Blank',testt$label_fspp)

test_labels <- testt %>% select(segment_record_id,label_record_id,label_f) %>% rename(segment_record_id_fk = segment_record_id, label_id_fk=label_f)



push_new_labels <- function(header,submission_records,chunksize){

  spl.dt <- split( submission_records , cut(1:nrow(submission_records), round(nrow(submission_records)/chunksize)))

  sendupatedlabels <- function(datachunk,header) {

      datachunk = jsonlite::toJSON(datachunk,pretty=TRUE)

      urlreq_ap <- httr2::req_url_path_append(header$root,"updateMediaLabels", header$key)
      urlreq_ap <- urlreq_ap |>  httr2::req_method("PUT")  |> httr2::req_body_json(jsonlite::fromJSON(datachunk))
      #
      preq <- httr2::req_perform(urlreq_ap)
      resp <- httr2::resp_body_string(preq)

      return(jsonlite::fromJSON(resp))
  }

  for (i in 1:length(spl.dt)){

    sendupatedlabels(datachunk=spl.dt[[i]],headers)
    message('submitted ',i*chunksize,' labels of ', nrow(submission_records))
  }

}

push_new_labels(headers,submission_records = test_labels,chunksize=30)
# devtools::load_all()









#


for (psri in 1:length(PSRIs)){

  ml <- get_media_assets(hdr=headers,
                         datatype="video",
                         psrID=PSRIs[psri])

  ml$label_f <- NA

  for (i in 1:nrow(ml)){
    lab <- ml$species[i]
    mfrl <- ml$media_file_reference_location[i]
    lab_correct <- DATA[DATA$new_vid_id==mfrl,"latin_name"][1]
    if (is.na(lab_correct)==T){
      lab_correct <- "Blank"
      BLANK <- c(BLANK,lab_correct)
      ml$label_f[i] <- NA
    }
    else{
      lab_correct2 <- strsplit(lab_correct, split="_")
      lab_correct3 <- paste(c(lab_correct2[[1]][1],lab_correct2[[1]][2]),collapse=" ")
      temp <- ifelse(is.na(PCLs$label_id[which(PCLs$label==lab_correct3)])==F,
                     PCLs$label_id[which(PCLs$label==lab_correct3)],
                     PCLs$label_id[which(PCLs$label==lab_correct3)])
      if (length(temp)>0){
        ml$label_f[i] <- temp
      }
      else{
        ADD_LABEL <- lab_correct3
        ml$label_f[i] <- NA
      }
    }
  }

  RES[[psri]] <- ml

  print(psri)
}

UPDATED_LABELS <- data.table::rbindlist(RES)





