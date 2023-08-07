##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 15 data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q15_increase_efficiency <- function(PLO_data_clean){
  
  PLO_data_clean |> 
    
  # select necessary cols ----
  select(increase_efficiency) |>
    
  # sum ----
  group_by(increase_efficiency) |>
    count() |>
    ungroup() |> 
    
  # ADDING BC NO ONE SELECTED THE FOLLOWING OPTIONS ----
  add_row(increase_efficiency = "3 (neutral)", n = 0) |>
    add_row(increase_efficiency = "2", n = 0) |>
    add_row(increase_efficiency = "1 (strongly disagree)", n = 0) |> 
    
  # reorder factors ----
  mutate(increase_efficiency = fct_relevel(increase_efficiency, 
                                           c("1 (strongly disagree)", "2", 
                                             "3 (neutral)", "4", "5 (strongly agree)"))) |>
    
  # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%")) |> 
    
  # create col with xvar name for plotting consistency ----
  mutate(xvar = increase_efficiency)
  
}
  