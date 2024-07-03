##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 8 data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q8_workflow_satisfaction <- function(PLO_data_clean){
  
  # to iterate over ----
  options <- c("Very Satisfied", "Satisfied", "Neutral", "Unsatisfied", "Very unsatisfied", "Not sure", "Never thought about this", "Not applicable")
  
  # select var of interest ----
  df1 <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(rate_satisfaction) |> 
    
    # sum ----
  group_by(rate_satisfaction) |> 
    count() |> 
    ungroup() 
  
  for (i in 1:length(options)){
    
    cat_name <- options[i]
    
    # if category already exists in df, skip to next one
    if (cat_name %in% pull(df1[,1])) {
      
      message(cat_name, " already exists. Moving to next option.")
      df1 <- df1
      
      # if category doesn't already exist, add it with n = 0 so that it still shows up on plot
    } else {
      
      message(cat_name, " does not exist. Adding now.")
      new_row <- data.frame(rate_satisfaction = cat_name, n = 0)
      df1 <- rbind(df1, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  # finish wrangling ----
  df2 <- df1 |> 
    
    # reorder factors ----
  mutate(rate_satisfaction = fct_relevel(rate_satisfaction,
                                        c("Not applicable", "Never thought about this", "Not sure",
                                          "Very unsatisfied", "Unsatisfied", "Neutral",
                                           "Satisfied", "Very Satisfied"))) |>
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%")) 
    
  # return final wrangled df
  return(df2)
  
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            plot Question 8 data                          ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q8_workflow_satisfaction <- function(q8_workflow_satisfaction_clean){
  
  ggplot(q8_workflow_satisfaction_clean, aes(x = rate_satisfaction, y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    coord_flip() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Satisfaction level",
         title = "Please rate your level of satisfaction with your current data\nmanagement and analysis workflow (i.e. how you collect, organize,\nstore, and analyze your data).",
         caption = "Question 8") +
    meds_theme()
  
}



