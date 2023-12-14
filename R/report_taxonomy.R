report_taxonomy <- function(new, old){
  #' Generate a lookup data frame where a vector 'old' has been transformed to a vector 'new' by a n:1 taxonomy.
  #' @param new A vector of length k
  #' @param old a vector of length k
  #'
  #' @returns data.frame
  
  frm <- data.frame(
    New = new,
    Old = old
  )
  
  old.frm <- data.frame(table(Old=old)) |>
    mutate(`Old Frequency` = Freq) |>
    select(-Freq)

  f <- function(frm, grp, .col){
    unique(frm)
  }
  
  frm |> 
    group_by(New) |>
    group_modify(f) |> 
    arrange(New, Old)  |>
    select(Old, New) |> 
    left_join(old.frm)

}
