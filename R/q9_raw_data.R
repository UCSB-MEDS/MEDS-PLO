##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 9 data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q9_raw_data <- function(PLO_data_clean){
  
  PLO_data_clean |>
    
  # select necessary cols ----
  select(raw_data) |>
    
  # sum ----
  group_by(raw_data) |>
    count() |>
    ungroup() |>
    
  # ADDING BC NO ONE SELECTED THE FOLLOWING OPTIONS ----
  add_row(raw_data = "3 (neutral)", n = 0) |>
    add_row(raw_data = "2", n = 0) |>
    add_row(raw_data = "1 (strongly disagree)", n = 0) |> 
    
  # reorder factors ----
  mutate(raw_data = fct_relevel(raw_data, 
                                c("1 (strongly disagree)", "2", 
                                  "3 (neutral)", "4", "5 (strongly agree)"))) |>
    
  # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%")) |> 
    
  # create col with xvar name for plotting consistency ----
  mutate(xvar = raw_data)
  
}
