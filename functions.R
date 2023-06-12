get_num_respondents <- function(df, var){
 
   num_respondents <- df |> 
      select(var) |>
      count() |>
      pull()
   
   return(num_respondents)
  
}
