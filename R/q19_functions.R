
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------------- CLEAN QUESTION 19C DATA-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q19c_fxn_output <- function(PLO_data_clean){
  
  #........................initial wrangling.......................
  df <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(fxn_output, timepoint) |>
    
    # sum ----
  group_by(timepoint, fxn_output) |>
    count() |>
    ungroup() 
  
  ##~~~~~~~~~~~~~~~~~~
  ##  ~ pre-MEDS  ----
  ##~~~~~~~~~~~~~~~~~~
  
  #.........separate pre-MEDS (to add 0s for missing cats).........
  pre_meds <- df |> 
    filter(timepoint == "Pre-MEDS") |> 
    mutate(total_respondents = sum(n),
           percentage = round((n/total_respondents)*100, 1),
           perc_label = paste0(percentage, "%")) |>
    mutate(xvar = fxn_output) |> 
    mutate(perc_label_long = paste0(perc_label, "\n(", n, "/", total_respondents, " respondents)"))
  
  ##~~~~~~~~~~~~~~~~~~~
  ##  ~ post-MEDS  ----
  ##~~~~~~~~~~~~~~~~~~~
  
  #........separate post-MEDS (to add 0s for missing cats).........
  post_meds <- df |> 
    filter(timepoint == "Post-MEDS") |> 
    mutate(total_respondents = sum(n),
           percentage = round((n/total_respondents)*100, 1),
           perc_label = paste0(percentage, "%")) |>
    mutate(xvar = fxn_output) |> 
    mutate(perc_label_long = paste0(perc_label, "\n(", n, "/", total_respondents, " respondents)"))
  
  ##~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ recombine dfs  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~
  
  all_q19c_data <- rbind(pre_meds, post_meds) |> 
    
    # filter only for correct answer ----
  filter(fxn_output == 10) 
  
  return(all_q19c_data)
  
}


