
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------------- CLEAN QUESTION 18B DATA-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q18b_prob_dist_terms <- function(PLO_data_clean){
  
  # ........................to iterate over.........................
  options <- c("normal", "bimodal", "uniform", "symmetric", "variable", "unequal", NA)
  
  #........................initial wrangling.......................
  df <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(prob_dist_terms, timepoint) |> 
    
    # split strings by `,` delim ----
  separate_longer_delim(prob_dist_terms, delim = ",") |> 
    
    # sum ----
  group_by(timepoint, prob_dist_terms) |> 
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
      new_row <- data.frame(timepoint = "Pre-MEDS", prob_dist_terms = cat_name, n = 0)
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
    mutate(xvar = prob_dist_terms)
  
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
      new_row <- data.frame(timepoint = "Post-MEDS", prob_dist_terms = cat_name, n = 0)
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
    mutate(xvar = prob_dist_terms)
  
  ##~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ recombine dfs  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~
  
  all_q18b_data <- rbind(pre_meds, post_meds) |> 
    mutate(xvar = fct_relevel(xvar,
                              c("unequal", "variable", "symmetric", "uniform", "bimodal", "normal"))) 
  
  return(all_q18b_data)
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##-------------------------- PLOT QUESTION 18B DATA-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q18b_prob_dist_terms <- function(data){

  ggplot(na.omit(data), aes(x = xvar, y = n, label = perc_label)) +
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
         title = "Which of the following terms are used to describe probability\ndistributions (select all that apply)?",
         subtitle = "Correct answer: normal, bimodal, uniform, symmetric",
         caption = "Question 18b") +
    facet_wrap(~timepoint) +
    meds_theme() +
    theme(
      legend.position = "none",
      axis.title.y = element_blank()
    )

}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------- CLEAN QUESTION 18B FULLY CORRECT------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q18b_FULLY_CORRECT <- function(PLO_data_clean){
  
  df <- PLO_data_clean |> 
    
    # select necessary cols ----
    select(prob_dist_terms, timepoint) |> 
    
    # sum ----
    group_by(timepoint, prob_dist_terms) |>
    
    mutate(correctness = ifelse(prob_dist_terms == "normal,uniform,bimodal,symmetric", 
                                yes = "yes",
                                no = "no")) 
  
  ##~~~~~~~~~~~~~~~~~~
  ##  ~ pre-MEDS  ----
  ##~~~~~~~~~~~~~~~~~~
  
  #....total respondents that continued onto answer question 18b...
  total_pre_resp <- meds2024_before_clean |> 
    select(prob_dist) |>
    group_by(prob_dist) |>
    # filter(prob_dist != "1 (never heard of it)") |> # include this if you want % calculation to be out of only students who advanced to this question
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
    select(prob_dist) |>
    group_by(prob_dist) |>
    # filter(prob_dist != "1 (never heard of it)") |> # include this if you want % calculation to be out of only students who advanced to this question
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
  
  all_q18b_fully_correct <- rbind(pre_meds, post_meds) |> 
    filter(correctness == "yes") |> 
    mutate(timepoint = fct_relevel(timepoint, c("Pre-MEDS", "Post-MEDS"))) 
  
  return(all_q18b_fully_correct)
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------- PLOT QUESTION 18B FULLY CORRECT-------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q18b_FULLY_CORRECT <- function(data){
  
  ggplot(data, aes(x = timepoint, y = percentage)) +
    geom_col(aes(fill = timepoint), position = position_dodge(preserve = "total")) +
    geom_text(aes(label = perc_label_long), 
              position = position_stack(vjust = 0.5), 
              size = 3, color = "white", family = "nunito") +
    labs(title = "% of respondents who correctly answered question 18b",
         subtitle = "(i.e. chose exactly the following options: normal, uniform, bimodal, symmetric)") +
    scale_fill_manual(values = meds_pal) +
    scale_y_continuous(labels = scales::label_percent(scale = 1),
                       limits = c(0, 100)) +
    meds_theme() +
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
  
}
  
