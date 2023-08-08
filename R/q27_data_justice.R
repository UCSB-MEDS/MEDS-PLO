##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 27 data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q27_data_justice <- function(PLO_data_clean){

  PLO_data_clean |> 
    
  # select necessary cols ----
  select(data_justice) |> 
    
  # sum ----
  group_by(data_justice) |>
    count() |>
    ungroup() |> 
    
  # ADDING BC NO ONE SELECTED THE FOLLOWING OPTIONS ----
  add_row(data_justice = "2", n = 0) |>
    
  # reorder factors ----
  mutate(data_justice = fct_relevel(data_justice, 
                                    c("1 (never heard of it)", "2", 
                                      "3 (vague sense of what it means)", "4", "5 (very familiar)"))) |>
    
  # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))  
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           plot Question 27 data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q27_data_justice <- function(data){
  
  ggplot(data, aes(x = data_justice, y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    coord_flip() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Familiarity level",
         title = " How familiar are you with the term Data Justice?",
         caption = "Question 27") +
    meds_theme
  
}

