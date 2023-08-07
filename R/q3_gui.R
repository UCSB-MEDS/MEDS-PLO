
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 3 data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

q3_gui_clean <- function(PLO_data_clean){
  
  PLO_data_clean |> 
    
  # select necessary cols ----
  select(point_and_click_gui) |> 
    
  # sum ----
  group_by(point_and_click_gui) |> 
    count() |> 
    ungroup() |> 
    
  # ADDING BC NO ONE SELECTED 'Daily' ----
  add_row(point_and_click_gui = "Daily", n = 0) |> 
    
  # reorder factors ----
  mutate(point_and_click_gui = fct_relevel(point_and_click_gui,
                                           c("Never", "Less than once per year", "Several times per year",
                                             "Monthly", "Weekly", "Daily"))) |>
    
  # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%")) |> 
  
  # create col with xvar name for plotting consistency ----
  mutate(xvar = point_and_click_gui)
    
}
