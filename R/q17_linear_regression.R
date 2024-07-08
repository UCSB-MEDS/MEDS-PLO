##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 17a data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for just one PLO assessment (pre or post)  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q17a_familiar_lr <- function(PLO_data_clean){
  
  # to iterate over ----
  options <- c("1 - never heard of it", "2", "3 - vague sense of what it means", "4", "5 - very familiar")
  
  df1 <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(linear_regression) |> 
    
    # sum ----
  group_by(linear_regression) |>
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
      new_row <- data.frame(linear_regression = cat_name, n = 0)
      df1 <- rbind(df1, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  # # ADDING BC NO ONE SELECTED THE FOLLOWING OPTIONS ----
  # add_row(linear_regression = "2", n = 0) |>
  # add_row(linear_regression = "1 - never heard of it", n = 0) |>
  
  # finish wrangling ----
  df2 <- df1 |> 
    
    # reorder factors ----
  mutate(linear_regression = fct_relevel(linear_regression, 
                                         c("1 - never heard of it", "2", 
                                           "3 - vague sense of what it means", "4", "5 - very familiar"))) |>
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for both pre & post assessments  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q17a_familiar_lr_bothPP <- function(PLO_data_clean){
  
  # ........................to iterate over.........................
  options <- c("1 - never heard of it", "2", "3 - vague sense of what it means", "4", "5 - very familiar")
  
  #........................initial wrangling.......................
  df <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(linear_regression, timepoint) |> 
    
    # sum ----
  group_by(timepoint, linear_regression) |> 
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
      new_row <- data.frame(timepoint = "Pre-MEDS", linear_regression = cat_name, n = 0)
      pre_meds <- rbind(pre_meds, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  #.................calculate pre-MEDS percentages.................
  pre_meds <- pre_meds |> 
    mutate(total_respondents = sum(n),
           percentage = round((n/total_respondents)*100, 1),
           perc_label = paste0(percentage, "%")) |>
    mutate(xvar = linear_regression)
  
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
      new_row <- data.frame(timepoint = "Post-MEDS", linear_regression = cat_name, n = 0)
      post_meds <- rbind(post_meds, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  #................calculate post-MEDS percentages.................
  post_meds <- post_meds |> 
    mutate(total_respondents = sum(n),
           percentage = round((n/total_respondents)*100, 1),
           perc_label = paste0(percentage, "%")) |>
    mutate(xvar = linear_regression)
  
  ##~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ recombine dfs  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~
  
  all_q17a_linear_regression <- rbind(pre_meds, post_meds) |> 
    mutate(linear_regression = fct_relevel(linear_regression,
                                           c("1 - never heard of it", "2", 
                                             "3 - vague sense of what it means", "4", 
                                             "5 - very familiar"))) 
  
  return(all_q17a_linear_regression)
  
} 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           plot Question 17a data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for just one PLO assessment (pre or post)  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q17a_familiar_lr <- function(data){
  
  ggplot(data, aes(x = linear_regression, y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    coord_flip() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Familiarity level",
         title = "How familiar are you with the term linear regression?",
         caption = "Question 17a (choosing '1 - never heard of it' skips respondent to question 18)") +
    meds_theme()
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for both pre & post assessments  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# see `plot_rank_bothPP()`

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 17b data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for just one PLO assessment (pre or post)  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q17b_microplastics <- function(PLO_data_clean){
  
  PLO_data_clean |> 
    
    # select necessary cols ----
  select(microplastics_lr) |> 
    
    # round values and remove sentences for plotting purposes ----
  mutate_if(is.character, str_replace_all, 
            pattern = "For 45 days of rain per year, we expect 27 pieces of microplastic, ceteris paribus.", replacement = "27") |> 
    
    # class 2023 after ----
  mutate_if(is.character, str_replace_all, pattern = "26.52", replacement = "27") |> 
    mutate_if(is.character, str_replace_all, pattern = "46.76823", replacement = "47") |> 
    mutate_if(is.character, str_replace_all, pattern = "46.77", replacement = "47") |> 
    mutate_if(is.character, str_replace_all, pattern = "900.28", replacement = "900") |> 
    mutate_if(is.character, str_replace_all, pattern = "47 mg", replacement = "47") |> 
    mutate_if(is.character, str_replace_all, pattern = "27 pieces", replacement = "27") |> 
    # class 2024 before ----
  mutate_if(is.character, str_replace_all, pattern = "Unsure", replacement = "I don't know") |> 
    mutate_if(is.character, str_replace_all, pattern = "i dont know", replacement = "I don't know") |> 
    mutate_if(is.character, str_replace_all, pattern = "no idea", replacement = "I don't know") |> 
    mutate_if(is.character, str_replace_all, pattern = "26.5", replacement = "27") |> 
    mutate_if(is.character, str_replace_all, pattern = "42.7", replacement = "43") |> 
    mutate_if(is.character, str_replace_all, pattern = "1.1511", replacement = "1") |> 
    mutate_if(is.character, str_replace_all, pattern = "0.58942", replacement = "1") |> 
    
    # convert to factor ----
  mutate(microplastics_lr = as_factor(microplastics_lr)) |> 
    
    # sum ----
  group_by(microplastics_lr) |>
    count() |>
    ungroup() |>
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for both pre & post assessments  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q17b_microplastics_bothPP <- function(PLO_data_clean){
  
  #........................initial wrangling.......................
  df <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(microplastics_lr, timepoint) |>
  
  # sum ----
  group_by(timepoint, microplastics_lr) |>
    count() |>
    ungroup() 
  
  ##~~~~~~~~~~~~~~~~~~
  ##  ~ pre-MEDS  ----
  ##~~~~~~~~~~~~~~~~~~
  
  #.........separate pre-MEDS (to add 0s for missing cats).........
  pre_meds <- df |> 
    filter(timepoint == "Pre-MEDS") |> 
    mutate(total_respondents = sum(n),
           percentage = round((n/total_respondents)*100, 1),
           perc_label = paste0(percentage, "%")) |>
    mutate(xvar = microplastics_lr) |> 
    mutate(perc_label_long = paste0(perc_label, " (", n, "/", total_respondents, " respondents)"))
  
  ##~~~~~~~~~~~~~~~~~~~
  ##  ~ post-MEDS  ----
  ##~~~~~~~~~~~~~~~~~~~
  
  #........separate post-MEDS (to add 0s for missing cats).........
  post_meds <- df |> 
    filter(timepoint == "Post-MEDS") |> 
    mutate(total_respondents = sum(n),
           percentage = round((n/total_respondents)*100, 1),
           perc_label = paste0(percentage, "%")) |>
    mutate(xvar = microplastics_lr) |> 
    mutate(perc_label_long = paste0(perc_label, "\n(", n, "/", total_respondents, " respondents)"))
  
  ##~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ recombine dfs  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~
  
  all_q17b_data <- rbind(pre_meds, post_meds) |> 
    filter(microplastics_lr == 47)
  
  return(all_q17b_data)
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           plot Question 17b data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for just one PLO assessment (pre or post)  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q17b_microplastics <- function(data){
  
  ggplot(data, aes(x = fct_reorder(microplastics_lr, desc(n)), y = n, label = perc_label, fill = microplastics_lr)) +
    geom_col() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Response",
         title = "How many pieces of microplastic do we predict will be present\nin a location with 45 days of rain per year (round your answer up to\nthe nearest integer)?",
         caption = "Question 17b (free response)") +
    scale_fill_manual(values = pal, limits = names(pal)) +
    meds_theme()
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for both pre & post assessments  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q17b_microplastics_bothPP <- function(data){
  
  ggplot(data, aes(x = timepoint, y = percentage)) +
    geom_col(aes(fill = timepoint)) +
    geom_text(aes(label = perc_label_long), 
              position = position_stack(vjust = 0.5), 
              size = 3, color = "white", family = "nunito") +
    labs(y = "% of respondents who\nanswered correctly",
         title = "How many pieces of microplastic do we predict will be present\nin a location with 45 days of rain per year (round your answer up\nto the nearest integer)?",
         subtitle = "Correct answer: 47",
         caption = "Question 17b (free response)") +
    scale_fill_manual(values = meds_pal) +
    scale_y_continuous(labels = scales::label_percent(scale = 1),
                       limits = c(0, 100)) +
    meds_theme() +
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      plot.subtitle = element_text(face = "bold")
    )
  
}


