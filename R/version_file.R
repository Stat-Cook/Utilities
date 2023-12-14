version_file <- function(prefix="Temp"){
  date.now <- as.Date(now())
  .bool <- TRUE
  
  while(.bool){  
    key <- paste0(sample(c(letters[1:6[]], 0:9), 16), collapse="")
    
    .file <- as.character(glue::glue(
      "{prefix}_{date.now}_{key}"
    ))
    
    .bool <- file.exists(.file)
  }
  
  .file

}

writeRDS_version <- function(object, prefix="Temp"){
  .version_file <- version_file(prefix)
  saveRDS(object, .version_file)
  .version_file
}
