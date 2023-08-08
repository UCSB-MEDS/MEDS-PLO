
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 6 data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q6_version_control <- function(PLO_data_clean){
  
  PLO_data_clean |> 
    
    # select necessary cols ----
  select(version_control) |>
    
  # sum ----
  group_by(version_control) |>
    count() |>
    ungroup() |> 
    
  # ADDING BC NO ONE SELECTED THE FOLLOWING OPTIONS ----
  add_row(version_control = "Monthly", n = 0) |> 
    add_row(version_control = "Several times per year", n = 0) |> 
    add_row(version_control = "Less than once per year", n = 0) |> 
    add_row(version_control = "Never", n = 0) |>
    
  # reorder factors ----
  mutate(version_control = fct_relevel(version_control, c("Never", "Less than once per year", "Several times per year",
                                                          "Monthly", "Weekly", "Daily"))) |>
    
  # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%")) |> 
    
  # create col with xvar name for plotting consistency ----
  mutate(xvar = version_control)
    
    
}