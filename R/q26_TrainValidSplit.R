
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##-------------------------- PLOT QUESTION 26C DATA-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q26c_mod_perf <- function(PLO_data_clean){
  
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
  
  if("Post-MEDS" %in% df$timepoint){
  
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
  
  } else{
    post_meds <- post_meds %>%
      mutate(total_respondents = 0,
             percentage = 0,
             perc_label = NA,
             learning_from_model = NA)
  }
  
  
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

plot_q26c_mod_perf <- function(data){
  
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

clean_q26c_FULLY_CORRECT <- function(PLO_data_clean){
  
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
  
  #....total respondents that continued onto answer question 26b...
  total_pre_resp <- both_timepoints_clean |> #meds2025_before_clean |>
    filter(timepoint == "Pre-MEDS") |>
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
  
  #....total respondents that continued onto answer question 26b...
  total_post_resp <- both_timepoints_clean |> #meds2025_after_clean |>
    filter(timepoint == "Post-MEDS") |>
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

plot_q26c_FULLY_CORRECT <- function(data){
  
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
