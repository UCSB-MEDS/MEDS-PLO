##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 8 data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

q8_workflow_satisfaction_clean <- function(PLO_data_clean){
  
  PLO_data_clean |> 
      
    # select necessary cols ----
    select(rate_satisfaction) |>
      
    # sum ----
    group_by(rate_satisfaction) |>
      count() |>
      ungroup() |>
      
    # ADDING BC NO ONE SELECTED THE FOLLOWING OPTIONS ----
    add_row(rate_satisfaction = "Very unsatisfied", n = 0) |>
      add_row(rate_satisfaction = "Not sure", n = 0) |>
      add_row(rate_satisfaction = "Never thought about this", n = 0) |>
      add_row(rate_satisfaction = "Not applicable", n = 0) |>
      
    # reorder factors ----
    mutate(rate_satisfaction = fct_relevel(rate_satisfaction, c("Not applicable", "Never thought about this", "Not sure",
                                                                "Very unsatisfied", "Unsatisfied", "Neutral",
                                                                "Satisfied", "Very Satisfied"))) |>
      
    # add col for percentages ----
    mutate(percentage = round((n/(sum(n)))*100, 1),
           perc_label = paste0(percentage, "%"))
  
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            plot Question 8 data                          ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

q8_workflow_satisfaction_plot <- function(q8_workflow_satisfaction_clean){
  
  ggplot(q8_workflow_satisfaction_clean, aes(x = rate_satisfaction, y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    coord_flip() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Satisfaction level",
         title = "Please rate your level of satisfaction with your current data\nmanagement and analysis workflow (i.e. how you collect, organize,\nstore, and analyze your data).",
         caption = "Question 8") +
    meds_theme
  
}



