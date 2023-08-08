##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 19a data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q19a_familiar_functions <- function(PLO_data_clean){
  
  PLO_data_clean |> 
    
  # select necessary cols ----
  select(term_function) |> 
    
  # sum ----
  group_by(term_function) |>
    count() |>
    ungroup() |> 
    
  # ADDING BC NO ONE SELECTED THE FOLLOWING OPTIONS ----
  add_row(term_function = "2", n = 0) |>
    add_row(term_function = "1 (never heard of it)", n = 0) |>
    
  # reorder factors ----
  mutate(term_function = fct_relevel(term_function, 
                                     c("1 (never heard of it)", "2", 
                                       "3 (vague sense of what it means)", "4", "5 (very familiar)"))) |>
    
  # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           plot Question 19a data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q19a_familiar_functions <- function(data){
  
  ggplot(data, aes(x = term_function, y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    coord_flip() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Familiarity level",
         title = "How familiar are you with the term 'function' as it relates to\nprogramming?",
         caption = "Question 19a (choosing '1 (never heard of it)' skips respondent to question 20)") +
    meds_theme
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 19b data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q19b_writing_functions <- function(PLO_data_clean){
  
  PLO_data_clean |> 
    
  # select necessary cols ----
  select(writing_functions) |> 
    
  # sum ----
  group_by(writing_functions) |>
    count() |>
    ungroup() |> 
    
  # ADDING BC NO ONE SELECTED THE FOLLOWING OPTIONS ----
  add_row(writing_functions = "1 (not at all)", n = 0) |>
    
  # reorder factors ----
  mutate(writing_functions = fct_relevel(writing_functions, 
                                         c("1 (not at all)", "2", 
                                           "3", "4", "5 (very comfortable)"))) |>
    
  # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           plot Question 19b data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q19b_writing_functions <- function(data){
  
  ggplot(data, aes(x = writing_functions, y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    coord_flip() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Comfort level",
         title = "How comfortable are you creating a function in code?",
         caption = "Question 19b (choosing '1 (not at all)' skips respondent to question 20)") +
    meds_theme
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 19c data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q19c_fxn_output <- function(PLO_data_clean){
  
  PLO_data_clean |> 
    
  # select necessary cols ----
  select(fxn_output) |> 
    
  # sum ----
  group_by(fxn_output) |>
    count() |>
    ungroup() |> 
    
  # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           plot Question 19c data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q19c_fxn_output <- function(data){
  
  ggplot(data, aes(x = fct_reorder(fxn_output, desc(n)), y = n, label = perc_label, fill = fxn_output)) +
    geom_col() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Selection",
         title = "What is the value of power_turbine_A?",
         caption = "Question 19c") +
    scale_fill_manual(values = pal, limits = names(pal)) +
    meds_theme

}


