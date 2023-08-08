##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 12 data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q12_overcoming_problems <- function(PLO_data_clean){
  
  PLO_data_clean |> 
    
  # select necessary cols ----
  select(overcoming_problems) |>
    
  # sum ----
  group_by(overcoming_problems) |>
    count() |>
    ungroup() |> 
    
  # ADDING BC NO ONE SELECTED THE FOLLOWING OPTIONS ----
  add_row(overcoming_problems = "3 (neutral)", n = 0) |>
    add_row(overcoming_problems = "1 (strongly disagree)", n = 0) |> 
    
  # reorder factors ----
  mutate(overcoming_problems = fct_relevel(overcoming_problems, 
                                           c("1 (strongly disagree)", "2", 
                                             "3 (neutral)", "4", "5 (strongly agree)"))) |>
    
  # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%")) |> 
    
  # create col with xvar name for plotting consistency ----
  mutate(xvar = overcoming_problems)
  
}
  