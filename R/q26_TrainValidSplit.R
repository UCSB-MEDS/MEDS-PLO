
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------------- CLEAN QUESTION 26A DATA-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for just one PLO assessment (pre or post)  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ compare pre & post assessments  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q26a_div_data_bothPP <- function(PLO_data_clean){
  
  # ........................to iterate over.........................
  options <- c("1 (never heard of it)", "2",
               "3 (vague sense of what it means)", 
               "4", "5 (very familiar)")
  
  #........................initial wrangling.......................
  df <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(ml_div_data, timepoint) |> 
    
    # sum ----
  group_by(timepoint, ml_div_data) |> 
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
    
    # if category already exists in df, skip to next one
    if (cat_name %in% pull(pre_meds[,2])) {
      
      message(cat_name, " already exists. Moving to next option.")
      pre_meds <- pre_meds
      
      # if category doesn't already exist, add it with n = 0 so that it still shows up on plot
    } else {
      
      message(cat_name, " does not exist. Adding now.")
      new_row <- data.frame(timepoint = "Pre-MEDS", ml_div_data = cat_name, n = 0)
      pre_meds <- rbind(pre_meds, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  #.................calculate pre-MEDS percentages.................
  pre_meds <- pre_meds |> 
    mutate(total_respondents = sum(n),
           percentage = round((n/total_respondents)*100, 1),
           perc_label = paste0(percentage, "%")) |>
    mutate(xvar = ml_div_data) 
  
  
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
      new_row <- data.frame(timepoint = "Post-MEDS", ml_div_data = cat_name, n = 0)
      post_meds <- rbind(post_meds, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  #................calculate post-MEDS percentages.................
  post_meds <- post_meds |> 
    mutate(total_respondents = sum(n),
           percentage = round((n/total_respondents)*100, 1),
           perc_label = paste0(percentage, "%")) |>
    mutate(xvar = ml_div_data)
  
  ##~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ recombine dfs  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~
  
  all_q22a_ml_div_data <- rbind(pre_meds, post_meds) |> 
    mutate(ml_div_data = fct_relevel(ml_div_data,
                                            c("1 (never heard of it)", "2",
                                              "3 (vague sense of what it means)", 
                                              "4", "5 (very familiar)")))
  
  
  return(all_q22a_ml_div_data)
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##-------------------------- PLOT QUESTION 26A DATA-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for just one PLO assessment (pre or post)  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q26a_div_data <- function(data){
  
  ggplot(data, aes(x = ml_div_data, y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    coord_flip() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Familiarity level",
         title = "How familiar are you with the procedure that divides your data into\nseparate “training”, “validation”, and “testing” sets?",
         caption = "Question 26a (choosing '1 (never heard of it)' skips respondent to question 27)") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    meds_theme()
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ compare pre & post assessments  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# see `plot_rank.R`

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------------- CLEAN QUESTION 26B DATA-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for just one PLO assessment (pre or post)  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ compare pre & post assessments  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q26b_tvs_bothPP <- function(PLO_data_clean){
  
  #........................to iterate over.........................
  options <- c("1 (never)", "2", "3", "4", "5 (all the time)")
  
  #........................initial wrangling.......................
  df <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(train_valid_split, timepoint) |> 
    
    # sum ----
  group_by(timepoint, train_valid_split) |> 
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
    
    # if category already exists in df, skip to next one
    if (cat_name %in% pull(pre_meds[,2])) {
      
      message(cat_name, " already exists. Moving to next option.")
      pre_meds <- pre_meds
      
      # if category doesn't already exist, add it with n = 0 so that it still shows up on plot
    } else {
      
      message(cat_name, " does not exist. Adding now.")
      new_row <- data.frame(timepoint = "Pre-MEDS", train_valid_split = cat_name, n = 0)
      pre_meds <- rbind(pre_meds, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  #.................calculate pre-MEDS percentages.................
  pre_meds <- pre_meds |> 
    mutate(total_respondents = sum(n),
           percentage = round((n/total_respondents)*100, 1),
           perc_label = paste0(percentage, "%")) |>
    mutate(xvar = train_valid_split) 
  
  
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
      new_row <- data.frame(timepoint = "Post-MEDS", train_valid_split = cat_name, n = 0)
      post_meds <- rbind(post_meds, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  #................calculate post-MEDS percentages.................
  post_meds <- post_meds |> 
    mutate(total_respondents = sum(n),
           percentage = round((n/total_respondents)*100, 1),
           perc_label = paste0(percentage, "%")) |>
    mutate(xvar = train_valid_split)
  
  ##~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ recombine dfs  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~
  
  all_q26b_data <- rbind(pre_meds, post_meds) |> 
    mutate(train_valid_split = fct_relevel(train_valid_split,
                                          c("1 (never)", "2", "3", "4", "5 (all the time)"))) |> 
    filter(xvar != "NULL")
  
  
  return(all_q26b_data)
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##-------------------------- PLOT QUESTION 26B DATA-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for just one PLO assessment (pre or post)  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q26b_tvs <- function(data){
  
  ggplot(data, aes(x = train_valid_split, y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    coord_flip() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Selection",
         title = "How often have you implemented a train, validation, test split?",
         caption = "Question 26b (choosing '1 (never)' skips respondent to question 27)") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    meds_theme()
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for both pre & post assessments  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# see `plot_rank.R`

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##-------------------------- PLOT QUESTION 26C DATA-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for just one PLO assessment (pre or post)  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ compare pre & post assessments  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q26c_mod_perf_bothPP <- function(PLO_data_clean){
  
  # ........................to iterate over.........................
  options <- c("My model is overfitting the training set",
               "My model is unlikely to perform well when applied to new data",
               "My model is likely to perform very well when applied to new data",
               "My test set has data entry errors in it",
               NA)
  
  #........................initial wrangling.......................
  df <- PLO_data_clean |> 
    
    # select necessary cols ----
    select(learning_from_model, timepoint) |> 
    
    # split strings by `,` delim ----
    separate_longer_delim(learning_from_model, delim = ",") |> 
    
    # sum ----
   group_by(timepoint, learning_from_model) |> 
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
    
    # if category already exists in df, skip to next one
    if (cat_name %in% pull(pre_meds[,2])) {
      
      message(cat_name, " already exists. Moving to next option.")
      pre_meds <- pre_meds
      
      # if category doesn't already exist, add it with n = 0 so that it still shows up on plot
    } else {
      
      message(cat_name, " does not exist. Adding now.")
      new_row <- data.frame(timepoint = "Pre-MEDS", learning_from_model = cat_name, n = 0)
      pre_meds <- rbind(pre_meds, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  # #....total respondents that continued onto answer question 18b...
  # total_pre_resp <- meds2024_before_clean |> 
  #   select(prob_dist) |>
  #   group_by(prob_dist) |>
  #   # filter(prob_dist != "1 (never heard of it)") |> # include this if you want % calculation to be out of only students who advanced to this question
  #   count() |>
  #   ungroup() |>
  #   summarize(n = sum(n)) |>
  #   pull()
  
  #.................calculate pre-MEDS percentages.................
  pre_meds <- pre_meds |> 
    mutate(total_respondents = pre_meds_num_respondents,
           percentage = round((n/total_respondents)*100, 1),
           perc_label = paste0(percentage, "%")) |>
    mutate(xvar = learning_from_model)
  
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
      new_row <- data.frame(timepoint = "Post-MEDS", learning_from_model = cat_name, n = 0)
      post_meds <- rbind(post_meds, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  # #....total respondents that continued onto answer question 18b...
  # total_post_resp <- meds2024_after_clean |>
  #   select(prob_dist) |>
  #   group_by(prob_dist) |>
  #   # filter(prob_dist != "1 (never heard of it)") |> # include this if you want % calculation to be out of only students who advanced to this question
  #   count() |>
  #   ungroup() |>
  #   summarize(n = sum(n)) |>
  #   pull()
  
  #................calculate post-MEDS percentages.................
  post_meds <- post_meds |> 
    mutate(total_respondents = post_meds_num_respondents,
           percentage = round((n/total_respondents)*100, 1),
           perc_label = paste0(percentage, "%")) |>
    mutate(xvar = learning_from_model)
  
  ##~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ recombine dfs  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~
  
  all_q26c_data <- rbind(pre_meds, post_meds) |> 
    mutate(xvar = fct_relevel(xvar,
                              c("My model is overfitting the training set",
                                "My model is unlikely to perform well when applied to new data",
                                "My model is likely to perform very well when applied to new data",
                                "My test set has data entry errors in it"))) 
  
  return(all_q26c_data)
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##-------------------------- PLOT QUESTION 26C DATA-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for just one PLO assessment (pre or post)  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q26c_mod_perf <- function(data){

  ggplot(data, aes(x = fct_reorder(learning_from_model, desc(n)), y = n, label = perc_label, fill = learning_from_model)) +
    geom_col() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Selection",
         title = "If my model performs very well in my training set, moderately well\nin my validation set, and poorly in my test set, what do I learn from\nthis (choose all that apply)?",
         caption = "Question 26c") +
    scale_fill_manual(values = pal, limits = names(pal)) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    meds_theme() +
    theme(
      legend.position = "none"
    )
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ compare pre & post assessments  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q26c_mod_perf_bothPP <- function(data){
  
  ggplot(na.omit(data), aes(x = fct_relevel(xvar,
                                            c("My test set has data entry errors in it",
                                              "My model is likely to perform very well when applied to new data",
                                              "My model is unlikely to perform well when applied to new data",
                                              "My model is overfitting the training set")), 
                            y = n, label = perc_label)) +
    geom_col(aes(fill = timepoint), position = position_dodge(preserve = "total")) +
    coord_flip() +
    geom_text( # see: https://cedricscherer.com/2023/10/26/yet-another-how-to-on-labelling-bar-graphs-in-ggplot2/
      aes(label = paste0("  ", sprintf("%2.1f", percentage), "%  "), 
          hjust = percentage > 3),
      position = position_dodge2(width = 0.9),
      size = 3, color = "white", family = "nunito"
    ) +
    scale_fill_manual(values = meds_pal) +
    labs(y = "Number of MEDS students",
         title = "If my model performs very well in my training set, moderately well\nin my validation set, and poorly in my test set, what do I learn from\nthis (choose all that apply)?",
         subtitle = "Correct answer: my model is overfitting the training set,\nmy model is unlikely to perform well when applied to new data",
         caption = "Question 26c") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
    facet_wrap(~timepoint) +
    meds_theme() +
    theme(
      legend.position = "blank",
      axis.title.y = element_blank()
    )
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------- CLEAN QUESTION 26C FULLY CORRECT------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for just one PLO assessment (pre or post)  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ compare pre & post assessments  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q26c_FULLY_CORRECT_bothPP <- function(PLO_data_clean){
  
  df <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(learning_from_model, timepoint) |> 
    
    # sum ----
  group_by(timepoint, learning_from_model) |>
    
    mutate(correctness = ifelse(learning_from_model == "My model is unlikely to perform well when applied to new data,My model is overfitting the training set", 
                                yes = "yes",
                                no = "no")) 
  
  ##~~~~~~~~~~~~~~~~~~
  ##  ~ pre-MEDS  ----
  ##~~~~~~~~~~~~~~~~~~
  
  #....total respondents that continued onto answer question 18b...
  total_pre_resp <- meds2024_before_clean |> 
    select(learning_from_model) |>
    group_by(learning_from_model) |>
    # filter(learning_from_model != "1 (never heard of it)") |> # include this if you want % calculation to be out of only students who advanced to this question
    count() |>
    ungroup() |>
    summarize(n = sum(n)) |>
    pull()
  
  pre_meds <- df |> 
    filter(timepoint == "Pre-MEDS") |> 
    group_by(correctness) |> 
    count() |> 
    # summarize(total = sum(n)) |> 
    # ungroup() |> 
    mutate(timepoint = rep("Pre-MEDS")) |> 
    mutate(total_respondents = total_pre_resp,
           percentage = round((n/total_respondents)*100, 1),
           perc_label = paste0(percentage, "%")) |> 
    mutate(perc_label_long = paste0(perc_label, " (", n, "/", total_respondents, " respondents)"))
  
  ##~~~~~~~~~~~~~~~~~~~
  ##  ~ post-MEDS  ----
  ##~~~~~~~~~~~~~~~~~~~
  
  #....total respondents that continued onto answer question 18b...
  total_post_resp <- meds2024_after_clean |> 
    select(learning_from_model) |>
    group_by(learning_from_model) |>
    # filter(learning_from_model != "1 (never heard of it)") |> # include this if you want % calculation to be out of only students who advanced to this question
    count() |>
    ungroup() |>
    summarize(n = sum(n)) |>
    pull()
  
  post_meds <- df |> 
    filter(timepoint == "Post-MEDS") |> 
    group_by(correctness) |> 
    count() |> 
    # summarize(total = sum(n)) |> 
    # ungroup() |> 
    mutate(timepoint = rep("Post-MEDS")) |> 
    mutate(total_respondents = total_post_resp,
           percentage = round((n/total_respondents)*100, 1),
           perc_label = paste0(percentage, "%")) |> 
    mutate(perc_label_long = paste0(perc_label, "\n(", n, "/", total_respondents, " respondents)"))
  
  ##~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ recombine dfs  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~
  
  all_q26c_data <- rbind(pre_meds, post_meds) |> 
    filter(correctness == "yes") |> 
    mutate(timepoint = fct_relevel(timepoint, c("Pre-MEDS", "Post-MEDS"))) 
  
  return(all_q26c_data)
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------- PLOT QUESTION 26C FULLY CORRECT-------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for just one PLO assessment (pre or post)  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q26c_FULLY_CORRECT <- function(data){
  
  ggplot(data, aes(x = fct_reorder(correctness, desc(total)), y = total, label = perc_label, fill = correctness)) +
    geom_col() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Did they get the question fully correct?",
         caption = "Question 26c") +
    scale_fill_manual(values = pal, limits = names(pal)) +
    meds_theme() +
    theme(
      legend.position = "none"
    )
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ compare pre & post assessments  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q26c_FULLY_CORRECT_bothPP <- function(data){
  
  ggplot(data, aes(x = timepoint, y = percentage)) +
    geom_col(aes(fill = timepoint), position = position_dodge(preserve = "total")) +
    geom_text(aes(label = perc_label_long), 
              position = position_stack(vjust = 0.5), 
              size = 3, color = "white", family = "nunito") +
    labs(title = "% of respondents who correctly answered question 26c",
         subtitle = "(i.e. chose exactly the following options: My model is likely to perform very well when applied to new data,\nMy model is unlikely to perform well when applied to new data)") +
    scale_fill_manual(values = meds_pal) +
    scale_y_continuous(labels = scales::label_percent(scale = 1),
                       limits = c(0, 100)) +
    meds_theme() +
    theme(
      legend.position = "blank",
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
  
}
