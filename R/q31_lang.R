##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 31 data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q31_lang <- function(PLO_data_clean){
  
  # to iterate over ----
  options <- c("I'm not sure", "Python", "R", "SQL")
  
  # select var of interest ----
  df1 <- PLO_data_clean |> 
    
  # select necessary cols ----
  select(what_lang_is_this) |> 
    
  # sum ----
  group_by(what_lang_is_this) |>
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
      new_row <- data.frame(what_lang_is_this = cat_name, n = 0)
      df1 <- rbind(df1, new_row)
      
    }
    
    message("----------------------")
    
  } 

  # finish wrangling ----
  df2 <- df1 |> 
    
  # reorder factors ----
  mutate(what_lang_is_this = fct_relevel(what_lang_is_this,
                                c("Python", "R", "SQL", "I'm not sure"))) |>
    
  # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           plot Question 31 data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q31_lang <- function(data){
  
  ggplot(data, aes(x = what_lang_is_this, y = n, label = perc_label, fill = what_lang_is_this)) +
    geom_col() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Language",
         title = "What programming language is the above code written in?",
         caption = "Question 31") +
    scale_fill_manual(values = pal, limits = names(pal)) +
    meds_theme()
  
}

