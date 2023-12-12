ChunkedFrame <- R6::R6Class(
  "ChunkedFrame",
  list(
    frm = NA,
    nrow= NA,
    index=NA,
    splits = NA, 
    
    initialize = function(frm, chunksize=10){
      self$frm <- frm
      self$nrow = nrow(frm)
      self$index <- NA
      
      self$split_frm()
    },
    split_frm = function(chunksize){
      self$index <- ceiling(seq(self$nrow) / 10)
      
      self$splits <- split(
        self$frm,
        self$index  
      )
    },
    
    clip = function(){
      split <- self$splits[[1]]
      clipr::write_clip(split)
      self$splits <- tail(self$splits, -1)
    }
  )
)

chunk_frm <- function(frm, chunksize=10){
  ChunkedFrame$new(frm, chunksize=chunksize)
}
