dryadFileDownloadID <- function(id, ext){
  download_url2 <- paste0("https://datadryad.org/api/v2/files/",id, "/download")
  temp <- tempfile(fileext=paste0(".", ext))
  download.file(download_url2, destfile = temp, mode = "wb")
  loaded_data <- switch(ext,
                        csv = read_csv(temp),
                        xlsx = read.xlsx(temp)
  )
  return(loaded_data)
}
