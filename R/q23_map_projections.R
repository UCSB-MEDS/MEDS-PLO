##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 23a data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q23a_comfort_map_proj <- function(PLO_data_clean){
  
  PLO_data_clean |> 
    
  # select necessary cols ----
  select(map_proj_comfort) |> 
    
  # sum ----
  group_by(map_proj_comfort) |>
    count() |>
    ungroup() |> 
    
  # coerce to factors ----
  mutate(map_proj_comfort = as_factor(map_proj_comfort)) |> 
    
  # reorder factors ----
  mutate(map_proj_comfort = fct_relevel(map_proj_comfort,
                                        c("1 (never worked with it before)", "2",
                                          "3", "4", "5 (work it with all the time)"))) |>
    
  # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           plot Question 23a data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q23a_comfort_map_proj <- function(data){
  
  ggplot(data, aes(x = map_proj_comfort, y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    coord_flip() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Comfort level",
         title = "How comfortable are you working with map projections?",
         caption = "Question 23a (choosing '1 (never worked with it before)' skips respondent to question 24)") +
    meds_theme
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 23b data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q23b_reproj <- function(PLO_data_clean){
  
  PLO_data_clean |>
    
  # select necessary cols ----
  select(convert_geo_to_coord) |>
    
  # sum ----
  group_by(convert_geo_to_coord) |>
    count() |>
    ungroup() |>
    
  # replace NA (from respondent who selected 'No' for previous question) ----
  replace_na(list(convert_geo_to_coord = "No response")) |> 
    
  # coerce to factors ----
  mutate(convert_geo_to_coord = as_factor(convert_geo_to_coord)) |>
    
  # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           plot Question 23b data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q23b_reproj <- function(data){
  
  ggplot(data, aes(x = fct_reorder(convert_geo_to_coord, desc(n)), y = n, label = perc_label, fill = convert_geo_to_coord)) +
    geom_col() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Selection",
         title = "Converting from a geographic to projected coordinate system\n reprojects data from ___ (fill in the blank)",
         caption = "Question 23b") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    scale_fill_manual(values = pal, limits = names(pal)) +
    meds_theme 
  
}


