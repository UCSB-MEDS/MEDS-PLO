##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 16a data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for just one PLO assessment (pre or post)  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q16a_median <- function(PLO_data_clean){
  
  PLO_data_clean |> 
    
  # select necessary cols ----
  select(median) |> 
    
  # coerce to factor ----
  mutate(median = as_factor(median)) |> 
    
  # sum ----
  group_by(median) |>
    count() |>
    ungroup() |> 
    
  # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for both pre & post assessments  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q16a_median_bothPP <- function(PLO_data_clean){

  #........................initial wrangling.......................
  df <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(median, timepoint) |> 
    
    # coerce to factor ----
  #mutate(median = as_factor(median)) |> 
    
    # sum ----
  group_by(timepoint, median) |>
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
           perc_label = paste0(percentage, "%"),
           perc_label_long = paste0(perc_label, "\n(", n, "/", total_respondents, " respondents)")) |>
    mutate(xvar = median)
  
  ##~~~~~~~~~~~~~~~~~~~
  ##  ~ post-MEDS  ----
  ##~~~~~~~~~~~~~~~~~~~
  
  #........separate post-MEDS (to add 0s for missing cats).........
  post_meds <- df |> 
    filter(timepoint == "Post-MEDS") |> 
    mutate(total_respondents = sum(n),
           percentage = round((n/total_respondents)*100, 1),
           perc_label = paste0(percentage, "%"),
           perc_label_long = paste0(perc_label, "\n(", n, "/", total_respondents, " respondents)")) |>
    mutate(xvar = median)
  
  ##~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ recombine dfs  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~
  
  all_q16a_median <- rbind(pre_meds, post_meds) |> 
    
    # filter only for correct answer ----
    filter(median == 14) 
  
  return(all_q16a_median)
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           plot Question 16a data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for just one PLO assessment (pre or post)  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q16a_median <- function(data){
  
  ggplot(data, aes(x = fct_reorder(median, desc(n)), y = n, label = perc_label, fill = median)) +
    geom_col() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Selection",
         title = "Calculate the median of this sample distribution: 5, 17, 0, 14, 14",
         subtitle = "Correct answer: 14",
         caption = "Question 16a (free response)") +
    scale_fill_manual(values = pal, limits = names(pal)) +
    meds_theme() +
    theme(
      legend.position = "none",
      plot.subtitle = element_text(face = "bold")
    )
  
}
  
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for both pre & post assessments  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# NOW USE `plot_correct_answer_comparison()`

# plot_q16a_median_bothPP <- function(data){
#   
#   ggplot(data, aes(x = timepoint, y = percentage)) +
#     geom_col(aes(fill = timepoint)) +
#     geom_text(aes(label = perc_label), 
#               position = position_stack(vjust = 0.5), 
#               size = 3, color = "white", family = "nunito") +
#     labs(y = "% of respondents who\nanswered correctly",
#          title = "Calculate the median of this sample distribution: 5, 17, 0, 14, 14",
#          subtitle = "Correct answer: 14",
#          caption = "Question 16a (free response)") +
#     scale_fill_manual(values = meds_pal) +
#     scale_y_continuous(labels = scales::label_percent(scale = 1)) +
#     meds_theme() +
#     theme(
#       legend.position = "none",
#       axis.title.x = element_blank(),
#       plot.subtitle = element_text(face = "bold")
#     )
#   
# }
  
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 16b data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for just one PLO assessment (pre or post)  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q16b_mode <- function(PLO_data_clean){
  
  PLO_data_clean |> 
    
  # select necessary cols ----
  select(mode) |> 
    
  # coerce to factor ----
  mutate(mode = as_factor(mode)) |> 
    
  # sum ----
  group_by(mode) |>
    count() |>
    ungroup() |> 
    
  # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for both pre & post assessments  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q16b_mode_bothPP <- function(PLO_data_clean){
  
  #........................initial wrangling.......................
  df <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(mode, timepoint) |> 
    
    # coerce to factor ----
  #mutate(median = as_factor(median)) |> 
  
  # sum ----
  group_by(timepoint, mode) |>
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
           perc_label = paste0(percentage, "%"),
           perc_label_long = paste0(perc_label, "\n(", n, "/", total_respondents, " respondents)")) |>
    mutate(xvar = mode)
  
  ##~~~~~~~~~~~~~~~~~~~
  ##  ~ post-MEDS  ----
  ##~~~~~~~~~~~~~~~~~~~
  
  #........separate post-MEDS (to add 0s for missing cats).........
  post_meds <- df |> 
    filter(timepoint == "Post-MEDS") |> 
    mutate(total_respondents = sum(n),
           percentage = round((n/total_respondents)*100, 1),
           perc_label = paste0(percentage, "%"),
           perc_label_long = paste0(perc_label, "\n(", n, "/", total_respondents, " respondents)")) |>
    mutate(xvar = mode)
  
  ##~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ recombine dfs  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~
  
  all_q16b_mode <- rbind(pre_meds, post_meds) |> 
    
    # filter only for correct answer ----
  filter(mode == 14) 
  
  return(all_q16b_mode)
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           plot Question 16b data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for just one PLO assessment (pre or post)  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q16b_mode <- function(data){
  
  ggplot(data, aes(x = fct_reorder(mode, desc(n)), y = n, label = perc_label, fill = mode)) +
    geom_col() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Selection",
         title = "Calculate the mode of this sample distribution: 5, 17, 0, 14, 14",
         caption = "Question 16b (free response)") +
    scale_fill_manual(values = pal, limits = names(pal)) +
    meds_theme() +
    theme(
      legend.position = "none"
    )
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for both pre & post assessments  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# plot_q16b_mode_bothPP <- function(data){
#   
#   ggplot(data, aes(x = timepoint, y = percentage)) +
#     geom_col(aes(fill = timepoint)) +
#     geom_text(aes(label = perc_label), 
#               position = position_stack(vjust = 0.5), 
#               size = 3, color = "white", family = "nunito") +
#     labs(y = "% of respondents who answered correctly",
#          title = "Calculate the mode of this sample distribution: 5, 17, 0, 14, 14",
#          subtitle = "Correct answer: 14",
#          caption = "Question 16b (free response)") +
#     scale_fill_manual(values = meds_pal) +
#     scale_y_continuous(labels = scales::label_percent(scale = 1)) +
#     meds_theme() +
#     theme(
#       legend.position = "none",
#       axis.title.x = element_blank(),
#       plot.subtitle = element_text(face = "bold")
#     )
#   
# }

