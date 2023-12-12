report_taxonomy <- function(new, old){
  frm <- data.frame(
    New = new,
    Old = old
  )
  
  frm |> 
    group_by(New) |>
    group_modify(f) |> 
    arrange(New, Old)  |>
    select(Old, New)

}
