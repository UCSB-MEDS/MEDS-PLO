##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 27 data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q27_data_justice <- function(PLO_data_clean){

  # to iterate over ----
  options <- c("1 (never heard of it)", "2", 
               "3 (vague sense of what it means)", "4", "5 (very familiar)")
  
  df1 <- PLO_data_clean |> 
    
  # select necessary cols ----
  select(data_justice) |> 
    
  # sum ----
  group_by(data_justice) |>
    count() |>
    ungroup() 
    
  # ADDING BC NO ONE SELECTED THE FOLLOWING OPTIONS ----
  # add_row(data_justice = "2", n = 0) |>
  for (i in 1:length(options)){
    
    cat_name <- options[i]
    
    # if category already exists in df, skip to next one
    if (cat_name %in% pull(df1[,1])) {
      
      message(cat_name, " already exists. Moving to next option.")
      df1 <- df1
      
      # if category doesn't already exist, add it with n = 0 so that it still shows up on plot
    } else {
      
      message(cat_name, " does not exist. Adding now.")
      new_row <- data.frame(data_justice = cat_name, n = 0)
      df1 <- rbind(df1, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  # finish wrangling ----
  df2 <- df1 |> 
    
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
    meds_theme()
  
}

