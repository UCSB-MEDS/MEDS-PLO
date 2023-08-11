##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 26a data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q26a_div_data <- function(PLO_data_clean){
  
  # to iterate over ----
  options <- c("1 (never heard of it)", "2",
               "3 (vague sense of what it means)", 
               "4", "5 (very familiar)")
  
  df1 <- PLO_data_clean |>
    
  # select necessary cols ----
  select(ml_div_data) |>
    
  # sum ----
  group_by(ml_div_data) |>
    count() |>
    ungroup() 
    
  # ADDING BC NO ONE SELECTED THE FOLLOWING OPTIONS ----
  # add_row(ml_div_data = "1 (never heard of it)", n = 0) |>
  #   add_row(ml_div_data = "2", n = 0) |>
  for (i in 1:length(options)){
    
    cat_name <- options[i]
    
    # if category already exists in df, skip to next one
    if (cat_name %in% pull(df1[,1])) {
      
      message(cat_name, " already exists. Moving to next option.")
      df1 <- df1
      
      # if category doesn't already exist, add it with n = 0 so that it still shows up on plot
    } else {
      
      message(cat_name, " does not exist. Adding now.")
      new_row <- data.frame(ml_div_data = cat_name, n = 0)
      df1 <- rbind(df1, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  # finish wrangling ----
  df2 <- df1 |> 
    
  # reorder factors ----
  mutate(ml_div_data = fct_relevel(ml_div_data,
                                   c("1 (never heard of it)", "2",
                                     "3 (vague sense of what it means)", 
                                     "4", "5 (very familiar)"))) |>
    
  # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           plot Question 26a data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q26a_div_data <- function(data){
  
  ggplot(data, aes(x = ml_div_data, y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    coord_flip() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Familiarity level",
         title = "How familiar are you with the procedure that divides your data into\nseparate “training”, “validation”, and “testing” sets?",
         caption = "Question 26a (choosing '1 (never heard of it)' skips respondent to question 27)") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    meds_theme
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 26b data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q26b_tvs <- function(PLO_data_clean){
 
  # to iterate over ----
  options <- c("1 (never)", "2", "3", "4", "5 (all the time)")
  
  df1 <- PLO_data_clean |>
    
  # select necessary cols ----
  select(train_valid_split) |>
    
  # sum ----
  group_by(train_valid_split) |>
    count() |>
    ungroup()
    
  # ADDING BC NO ONE SELECTED THE FOLLOWING OPTIONS ----
  # add_row(train_valid_split = "1 (never)", n = 0) |>
  for (i in 1:length(options)){
    
    cat_name <- options[i]
    
    # if category already exists in df, skip to next one
    if (cat_name %in% pull(df1[,1])) {
      
      message(cat_name, " already exists. Moving to next option.")
      df1 <- df1
      
      # if category doesn't already exist, add it with n = 0 so that it still shows up on plot
    } else {
      
      message(cat_name, " does not exist. Adding now.")
      new_row <- data.frame(train_valid_split = cat_name, n = 0)
      df1 <- rbind(df1, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  # finish wrangling ----
  df2 <- df1 |> 
    
  # reorder factors ----
  mutate(train_valid_split = fct_relevel(train_valid_split,
                                         c("1 (never)", "2", "3", "4", "5 (all the time)"))) |>
    
  # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           plot Question 26b data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q26b_tvs <- function(data){
  
  ggplot(data, aes(x = train_valid_split, y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    coord_flip() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Selection",
         title = "How often have you implemented a train, validation, test split?",
         caption = "Question 26b (choosing '1 (never)' skips respondent to question 27)") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    meds_theme
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 26c data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q26c_mod_perf <- function(PLO_data_clean){
  
  PLO_data_clean |>
    
  # select necessary cols ----
  select(learning_from_model) |>
    
  # split strings by `,` delim ----
  separate_longer_delim(learning_from_model, delim = ",") |>
    
  # sum ----
  group_by(learning_from_model) |>
    count() |>
    ungroup() |>
    
  # ADDING BC NO ONE SELECTED THE FOLLOWING OPTIONS ----
  add_row(learning_from_model = "My model is likely to perform very well when applied to new data", n = 0) |>
    add_row(learning_from_model = "My test set has data entry errors in it", n = 0) |>
    
  # add col for percentages ----
  mutate(percentage = round((n/q26c_num_answers)*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           plot Question 26c data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q26c_mod_perf <- function(data){

  ggplot(data, aes(x = fct_reorder(learning_from_model, desc(n)), y = n, label = perc_label, fill = learning_from_model)) +
    geom_col() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Selection",
         title = "If my model performs very well in my training set, moderately well\nin my validation set, and poorly in my test set, what do I learn from\nthis (choose all that apply)?",
         caption = "Question 26c") +
    scale_fill_manual(values = pal, limits = names(pal)) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    meds_theme  
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                      clean Question 26c FULLY CORRECT                    ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q26c_FULLY_CORRECT <- function(PLO_data_clean){
  
  PLO_data_clean |>
    
  # select necessary cols ----
  select(learning_from_model) |>
    
  # sum ----
  group_by(learning_from_model) |>
    count() |>
    ungroup() |>
    
  # add correct or incorrect label ----
  mutate(correctness = case_when(
    learning_from_model == "My model is overfitting the training set" ~ "no",
    learning_from_model == "My model is unlikely to perform well when applied to new data,My model is overfitting the training set" ~ "yes",
  )) |>
    
  # coerce data types ----
  mutate(correctness = as_factor(correctness)) |>
    
  # sum ----
  group_by(correctness) |>
    summarize(total = sum(n)) |>
    ungroup() |>
    
  # add col for percentages ----
  mutate(percentage = round((total/(sum(total)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                      plot Question 26b FULLY CORRECT                     ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q26c_FULLY_CORRECT <- function(data){
  
  ggplot(data, aes(x = fct_reorder(correctness, desc(total)), y = total, label = perc_label, fill = correctness)) +
    geom_col() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Did they get the question fully correct?",
         caption = "Question 26c") +
    scale_fill_manual(values = pal, limits = names(pal)) +
    meds_theme
  
}


