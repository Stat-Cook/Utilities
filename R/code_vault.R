folder_to_vault <- function(files=".", 
                            pattern="Temp",
                            vault="../analysis_vault"
                            ){
  #' Freeze current repo to a tarball.
  #' 
  #' By default the function will look for a folder one level above the 
  #' working directory called 'analysis_vault'.  
  #' 
  #' @export
  hex <- c(0:9, letters[1:6])  
  .bool <- TRUE
  
  while(.bool){
    hex_sample <- sample(hex, 8)
    hex_affix <- paste0(hex_sample, collapse="")
    date <- as.Date(lubridate::now())
    
    .file <- glue::glue("{pattern}_{date}_{hex_affix}.tar")
    .fp <- file.path(vault, .file)
    
    .bool <- file.exists(.fp)
  }
  
  manifest_path <- file.path(vault, "manifest")
  writeLines(
    glue::glue("Created {.fp}"),
    manifest_path
  )
  
  tar(.fp, files)
  .fp
}

folder_from_vault <- function(.tar, .exdir=NULL){
  #' Unpack a tarball
  #' 
  #' @export
  if (is.null(.exdir)){
    .exdir <- stringr::str_replace(.tar, ".tar$", "")
  }
  
  untar(.tar,
        exdir = .exdir
  )
}
