
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------------- CLEAN QUESTION 8 DATA-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q8_workflow_sat <- function(PLO_data_clean){
  
  ##........................to iterate over.........................
  options <- c("Very Satisfied", "Satisfied", "Neutral", "Unsatisfied", "Very unsatisfied", "Not sure", "Never thought about this", "Not applicable")
  
  #........................initial wrangling.......................
  df <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(rate_satisfaction, timepoint) |> 
    
    # sum ----
  group_by(timepoint, rate_satisfaction) |> 
    count() |> 
    ungroup() 
  
  ##~~~~~~~~~~~~~~~~~~
  ##  ~ pre-MEDS  ----
  ##~~~~~~~~~~~~~~~~~~
  
  #.........separate pre-MEDS (to add 0s for missing cats).........
  pre_meds <- df |> 
    filter(timepoint == "Pre-MEDS") 
  
  #................add 0s where missing (pre-MEDS).................
  for (i in 1:length(options)){
    
    cat_name <- options[i]
    
    # if category already exists in df, skip to next one ----
    if (cat_name %in% pull(pre_meds[,2])) {
      
      message(cat_name, " already exists. Moving to next option.")
      pre_meds <- pre_meds
      
      # if category doesn't already exist, add it with n = 0 so that it still shows up on plot ----
    } else {
      
      message(cat_name, " does not exist. Adding now.")
      new_row <- data.frame(timepoint = "Pre-MEDS", rate_satisfaction = cat_name, n = 0)
      pre_meds <- pre_meds
      
    }
    
    message("----------------------")
    
  } 
  
  #.................calculate pre-MEDS percentages.................
  pre_meds <- pre_meds |> 
    mutate(total_respondents = sum(n),
           percentage = round((n/total_respondents)*100, 1),
           perc_label = paste0(percentage, "%")) |>
    mutate(xvar = rate_satisfaction)
  
  ##~~~~~~~~~~~~~~~~~~~
  ##  ~ post-MEDS  ----
  ##~~~~~~~~~~~~~~~~~~~
  
  #........separate post-MEDS (to add 0s for missing cats).........
  post_meds <- df |> 
    filter(timepoint == "Post-MEDS") 
  
  #................add 0s where missing (post-MEDS)................
  for (i in 1:length(options)){
    
    cat_name <- options[i]
    
    # if category already exists in df, skip to next one ----
    if (cat_name %in% pull(post_meds[,2])) {
      
      message(cat_name, " already exists. Moving to next option.")
      post_meds <- post_meds
      
      # if category doesn't already exist, add it with n = 0 so that it still shows up on plot ----
    } else {
      
      message(cat_name, " does not exist. Adding now.")
      new_row <- data.frame(timepoint = "Post-MEDS", rate_satisfaction = cat_name, n = 0)
      post_meds <- rbind(post_meds, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  #................calculate post-MEDS percentages.................
  post_meds <- post_meds |> 
    mutate(total_respondents = sum(n),
           percentage = round((n/total_respondents)*100, 1),
           perc_label = paste0(percentage, "%")) |>
    mutate(xvar = rate_satisfaction)
  
  ##~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ recombine dfs  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~
  
  all_q8_data <- rbind(pre_meds, post_meds) |> 
    mutate(rate_satisfaction = fct_relevel(rate_satisfaction,
                                           c("Not applicable", "Never thought about this", "Not sure",
                                             "Very unsatisfied", "Unsatisfied", "Neutral",
                                             "Satisfied", "Very Satisfied")))
  
  return(all_q8_data)
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------------- PLOT QUESTION 8 DATA------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q8_workflow_sat <- function(q8_workflow_satisfaction_clean){
  
  ggplot(q8_workflow_satisfaction_clean, aes(x = rate_satisfaction, y = n, label = perc_label)) +
    geom_col(aes(fill = timepoint), position = position_dodge(preserve = "total")) +
    coord_flip() +
    geom_text( # see: https://cedricscherer.com/2023/10/26/yet-another-how-to-on-labelling-bar-graphs-in-ggplot2/
      aes(label = paste0("  ", sprintf("%2.1f", percentage), "%  "), 
          hjust = percentage > 3),
      position = position_dodge2(width = 0.9),
      size = 3, color = "white", family = "nunito"
    ) +
    #geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    scale_fill_manual(values = meds_pal) +
    labs(y = "Number of MEDS students", x = "Satisfaction level",
         title = "Please rate your level of satisfaction with your current data\nmanagement and analysis workflow (i.e. how you collect, organize,\nstore, and analyze your data).",
         caption = "Question 8") +
    facet_wrap(~timepoint) +
    meds_theme() +
    theme(
      legend.position = "blank",
      axis.title.y = element_blank()
    )
  
}

