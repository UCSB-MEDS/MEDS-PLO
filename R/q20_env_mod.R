##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------------- CLEAN QUESTION 20A DATA-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q20a_run_env_mod <- function(PLO_data_clean){
  
  #........................initial wrangling.......................
  df <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(run_environ_model, timepoint) |>
    
    # sum ----
  group_by(timepoint, run_environ_model) |>
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
    mutate(xvar = run_environ_model) |> 
    mutate(perc_label_long = paste0(perc_label, "\n(", n, "/", total_respondents, " respondents)"))
  
  ##~~~~~~~~~~~~~~~~~~~
  ##  ~ post-MEDS  ----
  ##~~~~~~~~~~~~~~~~~~~
  
  #........separate post-MEDS (to add 0s for missing cats).........
  post_meds <- df |> 
    filter(timepoint == "Post-MEDS") |> 
    mutate(total_respondents = sum(n),
           percentage = round((n/total_respondents)*100, 1),
           perc_label = paste0(percentage, "%")) |>
    mutate(xvar = run_environ_model) |> 
    mutate(perc_label_long = paste0(perc_label, "\n(", n, "/", total_respondents, " respondents)"))
  
  ##~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ recombine dfs  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~
  
  all_q20a_data <- rbind(pre_meds, post_meds) |> 
    
    # filter only for correct answer ----
  filter(run_environ_model == "Yes") 
  
  return(all_q20a_data)
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##-------------------------- PLOT QUESTION 20A DATA-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q20a_run_env_mod <- function(data){
  
  ggplot(data, aes(x = timepoint, y = percentage)) +
    geom_col(aes(fill = timepoint)) +
    geom_text(aes(label = perc_label_long), 
              position = position_stack(vjust = 0.5), 
              size = 3, color = "white", family = "nunito") +
    labs(y = "% of respondents who\nanswered 'Yes'",
         title = "Have you run a model to learn something about (or predict\nsomething about) the environment?",
         caption = "Question 20a") +
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

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------------- CLEAN QUESTION 20B DATA-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q20b_sa <- function(PLO_data_clean){
  
  #........................initial wrangling.......................
  df <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(sensitivity_analysis, timepoint) |>
    
    # sum ----
  group_by(timepoint, sensitivity_analysis) |>
    count() |>
    ungroup() 
  
  ##~~~~~~~~~~~~~~~~~~
  ##  ~ pre-MEDS  ----
  ##~~~~~~~~~~~~~~~~~~
  
  #.........separate pre-MEDS (to add 0s for missing cats).........
  pre_meds <- df |> 
    filter(timepoint == "Pre-MEDS") |> 
    # drop_na() |> # include this if you want % calculation to be out of only students who advanced to this question
    mutate(total_respondents = sum(n),
           percentage = round((n/total_respondents)*100, 1),
           perc_label = paste0(percentage, "%")) |>
    mutate(xvar = sensitivity_analysis) |> 
    mutate(perc_label_long = paste0(perc_label, "\n(", n, "/", total_respondents, " respondents)"))
  
  ##~~~~~~~~~~~~~~~~~~~
  ##  ~ post-MEDS  ----
  ##~~~~~~~~~~~~~~~~~~~
  
  #........separate post-MEDS (to add 0s for missing cats).........
  post_meds <- df |> 
    filter(timepoint == "Post-MEDS") |> 
    # drop_na() |> # include this if you want % calculation to be out of only students who advanced to this question
    mutate(total_respondents = sum(n),
           percentage = round((n/total_respondents)*100, 1),
           perc_label = paste0(percentage, "%")) |>
    mutate(xvar = sensitivity_analysis) |> 
    mutate(perc_label_long = paste0(perc_label, "\n(", n, "/", total_respondents, " respondents)"))
  
  ##~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ recombine dfs  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~
  
  all_q20a_data <- rbind(pre_meds, post_meds) |> 
    filter(sensitivity_analysis == "Yes") 
  
  return(all_q20a_data)
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##-------------------------- PLOT QUESTION 20B DATA-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q20b_sa <- function(data){
  
  ggplot(data, aes(x = timepoint, y = percentage)) +
    geom_col(aes(fill = timepoint)) +
    geom_text(aes(label = perc_label_long), 
              position = position_stack(vjust = 0.5), 
              size = 3, color = "white", family = "nunito") +
    labs(y = "% of respondents who\nanswered 'Yes'",
         title = "Have you done a sensitivity analysis to assess how model results\nchange with changes in inputs or parameters?",
         caption = "Question 20b (choosing 'No' skips respondent to question 21)") +
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

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------------- CLEAN QUESTION 20C DATA-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q20c_param_int <- function(PLO_data_clean){
  
  #........................initial wrangling.......................
  df <- PLO_data_clean |> 
    
    # select necessary cols ----
    select(param_interactions, timepoint) |>
    
    # sum ----
    group_by(timepoint, param_interactions) |>
    count() |>
    ungroup() 
  
  ##~~~~~~~~~~~~~~~~~~
  ##  ~ pre-MEDS  ----
  ##~~~~~~~~~~~~~~~~~~
  
  #.........separate pre-MEDS (to add 0s for missing cats).........
  pre_meds <- df |> 
    filter(timepoint == "Pre-MEDS") |> 
    # drop_na() |> # include this if you want % calculation to be out of only students who advanced to this question
    mutate(total_respondents = sum(n),
           percentage = round((n/total_respondents)*100, 1),
           perc_label = paste0(percentage, "%")) |>
    mutate(xvar = param_interactions) |> 
    mutate(perc_label_long = paste0(perc_label, " (", n, "/", total_respondents, " respondents)"))
  
  ##~~~~~~~~~~~~~~~~~~~
  ##  ~ post-MEDS  ----
  ##~~~~~~~~~~~~~~~~~~~
  
  #........separate post-MEDS (to add 0s for missing cats).........
  post_meds <- df |> 
    filter(timepoint == "Post-MEDS") |> 
    # drop_na() |> # include this if you want % calculation to be out of only students who advanced to this question
    mutate(total_respondents = sum(n),
           percentage = round((n/total_respondents)*100, 1),
           perc_label = paste0(percentage, "%")) |>
    mutate(xvar = param_interactions) |> 
    mutate(perc_label_long = paste0(perc_label, "\n(", n, "/", total_respondents, " respondents)"))
  
  #~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ recombine dfs  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~
  
  all_q20c_data <- rbind(pre_meds, post_meds) |> 
    filter(param_interactions == "a global sensitivity analysis") 
  
  return(all_q20c_data)
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##-------------------------- PLOT QUESTION 20C DATA-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q20c_param_int <- function(data){
  
  ggplot(data, aes(x = timepoint, y = percentage)) +
    geom_col(aes(fill = timepoint)) +
    geom_text(aes(label = perc_label_long), 
              position = position_stack(vjust = 0.5), 
              size = 3, color = "white", family = "nunito") +
    labs(y = "% of respondents who\nanswered correctly",
         title = "If you want to explore how parameter interactions impact model\nresults, you would do...",
         subtitle = "Correct answer: a global sensitivity analysis",
         caption = "Question 20c") +
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
  
