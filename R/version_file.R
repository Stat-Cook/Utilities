version_file <- function(prefix="Temp"){
  date.now <- as.Date(now())
  key <- paste0(sample(c(letters[1:6[]], 0:9), 16), collapse="")

  as.character(glue::glue(
    "{prefix}_{date.now}_{key}"
  ))
}
