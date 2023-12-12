ChunkedFrame <- R6::R6Class(
  "ChunkedFrame",
  list(
    frm = NA,
    nrow= NA,
    index=NA,
    splits = NA, 
    n.chunks = NA,
    
    initialize = function(frm, chunksize=10){
      self$frm <- frm
      self$nrow = nrow(frm)
      self$index <- NA
      
      self$split_frm()
    },
    split_frm = function(chunksize){
      self$index <- ceiling(seq(self$nrow) / 10)
      self$n.chunks <- length(unique(self$index))

      self$splits <- split(
        self$frm,
        self$index  
      )
    },
    
    clip = function(){
      print(self)
      split <- self$splits[[1]]
      clipr::write_clip(split)
      self$splits <- tail(self$splits, -1)
    },
    
    print = function(...){
      if (length(self$splits) == 0){
        i <- "NA"
      } else {
        i <- self$n.chunks - length(self$splits) + 1
      }
      
      cat("ChunkedFrame[")
      cat(glue::glue("chunk {i} of {self$n.chunks}"))
      cat("]")
    }  
    
  )
)

chunk_frm <- function(frm, chunksize=10){
  ChunkedFrame$new(frm, chunksize=chunksize)
}
