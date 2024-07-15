
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------------- CLEAN QUESTION 23B DATA-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q23b_reproj <- function(PLO_data_clean){
  
  #........................initial wrangling.......................
  df <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(convert_geo_to_coord, timepoint) |>
    
    # sum ----
  group_by(timepoint, convert_geo_to_coord) |>
    count() |>
    ungroup() 
  
  ##~~~~~~~~~~~~~~~~~~
  ##  ~ pre-MEDS  ----
  ##~~~~~~~~~~~~~~~~~~
  
  #.........separate pre-MEDS (to add 0s for missing cats).........
  pre_meds <- df |> 
    filter(timepoint == "Pre-MEDS") |> 
    # drop_na() |> # include this if you want % calculation to be out of only students who advanced to this question
    mutate(total_respondents = sum(n),
           percentage = round((n/total_respondents)*100, 1),
           perc_label = paste0(percentage, "%")) |>
    mutate(xvar = convert_geo_to_coord) |> 
    mutate(perc_label_long = paste0(perc_label, "\n(", n, "/", total_respondents, " respondents)"))
  
  ##~~~~~~~~~~~~~~~~~~~
  ##  ~ post-MEDS  ----
  ##~~~~~~~~~~~~~~~~~~~
  
  #........separate post-MEDS (to add 0s for missing cats).........
  post_meds <- df |> 
    filter(timepoint == "Post-MEDS") |> 
    # drop_na() |> # include this if you want % calculation to be out of only students who advanced to this question
    mutate(total_respondents = sum(n),
           percentage = round((n/total_respondents)*100, 1),
           perc_label = paste0(percentage, "%")) |>
    mutate(xvar = convert_geo_to_coord) |> 
    mutate(perc_label_long = paste0(perc_label, "\n(", n, "/", total_respondents, " respondents)"))
  
  #~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ recombine dfs  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~
  
  all_q23b_data <- rbind(pre_meds, post_meds) |> 
    filter(convert_geo_to_coord == "3D to 2D") 
  
  return(all_q23b_data)
  
}

