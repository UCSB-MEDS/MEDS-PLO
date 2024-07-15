
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------------- CLEAN QUESTION 25C DATA-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q25c_kmeans <- function(PLO_data_clean){
  
  #........................initial wrangling.......................
  df <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(kmeans, timepoint) |>
    
    # sum ----
  group_by(timepoint, kmeans) |>
    count() |>
    ungroup() 
  
  ##~~~~~~~~~~~~~~~~~~
  ##  ~ pre-MEDS  ----
  ##~~~~~~~~~~~~~~~~~~
  
  #.........separate pre-MEDS (to add 0s for missing cats).........
  pre_meds <- df |> 
    filter(timepoint == "Pre-MEDS") |> 
    #drop_na() |> # include this if you want % calculation to be out of only students who advanced to this question
    mutate(total_respondents = sum(n),
           percentage = round((n/total_respondents)*100, 1),
           perc_label = paste0(percentage, "%"),
           perc_label_long = paste0(perc_label, " (", n, "/", total_respondents, " respondents)")) |>
    mutate(xvar = kmeans)
  
  ##~~~~~~~~~~~~~~~~~~~
  ##  ~ post-MEDS  ----
  ##~~~~~~~~~~~~~~~~~~~
  
  #........separate post-MEDS (to add 0s for missing cats).........
  post_meds <- df |> 
    filter(timepoint == "Post-MEDS") |> 
    # drop_na() |> # include this if you want % calculation to be out of only students who advanced to this question
    mutate(total_respondents = sum(n),
           percentage = round((n/total_respondents)*100, 1),
           perc_label = paste0(percentage, "%"),
           perc_label_long = paste0(perc_label, "\n(", n, "/", total_respondents, " respondents)")) |>
    mutate(xvar = kmeans)
  
  #~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ recombine dfs  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~
  
  all_q25c_data <- rbind(pre_meds, post_meds) |> 
    filter(kmeans == "unsupervised, does not require expert labeling of data") 
  
  return(all_q25c_data)
  
}
