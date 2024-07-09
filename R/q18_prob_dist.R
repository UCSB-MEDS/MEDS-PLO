##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 18a data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for just one PLO assessment (pre or post)  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q18a_familiar_prob_dist <- function(PLO_data_clean){
  
  # to iterate over ----
  options <- c("1 (never heard of it)", "2", "3 (vague sense of what it means)", "4", "5 (very familiar)")
  
  df1 <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(prob_dist) |> 
    
    # sum ----
  group_by(prob_dist) |>
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
      new_row <- data.frame(prob_dist = cat_name, n = 0)
      df1 <- rbind(df1, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  #   # ONLY ADDING BC NO ONE SELECTED THE FOLLOWING OPTIONS ----
  # add_row(prob_dist = "2", n = 0) |>
  #   add_row(prob_dist = "1 (never heard of it)", n = 0) |>
  
  # finish wrangling ----
  df2 <- df1 |> 
    
    # reorder factors ----
  mutate(prob_dist = fct_relevel(prob_dist, 
                                 c("1 (never heard of it)", "2", 
                                   "3 (vague sense of what it means)", "4", "5 (very familiar)"))) |>
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for both pre & post assessments  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q18a_familiar_prob_dist_bothPP <- function(PLO_data_clean){
  
  # ........................to iterate over.........................
  options <- c("1 (never heard of it)", "2", "3 (vague sense of what it means)", "4", "5 (very familiar)")
  
  #........................initial wrangling.......................
  df <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(prob_dist, timepoint) |> 
    
    # sum ----
  group_by(timepoint, prob_dist) |> 
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
      new_row <- data.frame(timepoint = "Pre-MEDS", prob_dist = cat_name, n = 0)
      pre_meds <- rbind(pre_meds, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  #.................calculate pre-MEDS percentages.................
  pre_meds <- pre_meds |> 
    mutate(total_respondents = sum(n),
           percentage = round((n/total_respondents)*100, 1),
           perc_label = paste0(percentage, "%")) |>
    mutate(xvar = prob_dist)
  
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
      new_row <- data.frame(timepoint = "Post-MEDS", prob_dist = cat_name, n = 0)
      post_meds <- rbind(post_meds, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  #................calculate post-MEDS percentages.................
  post_meds <- post_meds |> 
    mutate(total_respondents = sum(n),
           percentage = round((n/total_respondents)*100, 1),
           perc_label = paste0(percentage, "%")) |>
    mutate(xvar = prob_dist)
  
  ##~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ recombine dfs  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~
  
  all_q18a_prob_dist <- rbind(pre_meds, post_meds) |> 
    mutate(prob_dist = fct_relevel(prob_dist,
                                   c("1 (never heard of it)", "2", 
                                     "3 (vague sense of what it means)", 
                                     "4", "5 (very familiar)"))) 
  
  return(all_q18a_prob_dist)
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           plot Question 18a data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for just one PLO assessment (pre or post)  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q18a_familiar_prob_dist <- function(data){
  
  ggplot(data, aes(x = prob_dist, y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    coord_flip() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Familiarity level",
         title = "How familiar are you with the term probability distribution?",
         caption = "Question 18a (choosing '1 (never heard of it)' skips respondent to question 19)") +
    meds_theme()
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for both pre & post assessments  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# see `plot_rank.qmd`

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 18b data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for just one PLO assessment (pre or post)  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# DOES NOT CORRECTLY CALCULATE; DO NOT USE!

clean_q18b_prob_dist_terms <- function(PLO_data_clean){
  
  PLO_data_clean |> 
    
    # select necessary cols ----
  select(prob_dist_terms) |> 
    
    # split strings by `,` delim ----
  separate_longer_delim(prob_dist_terms, delim = ",") |> 
    
    # sum ----
  group_by(prob_dist_terms) |>
    count() |>
    ungroup() |> 
    
    # add col for percentages ----
  mutate(percentage = round((n/q18b_num_answers)*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for both pre & post assessments  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q18b_prob_dist_terms_bothPP <- function(PLO_data_clean){
  
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
##                           plot Question 18b data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for just one PLO assessment (pre or post)  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q18b_prob_dist_terms <- function(data){
  
  ggplot(data, aes(x = fct_reorder(prob_dist_terms, desc(n)), y = n, label = perc_label, fill = prob_dist_terms)) +
    geom_col() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Selection",
         title = "Which of the following terms are used to describe probability\ndistributions (select all that apply)?",
         caption = "Question 18b") +
    scale_fill_manual(values = pal, limits = names(pal)) +
    meds_theme()
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for both pre & post assessments  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q18b_prob_dist_terms_bothPP <- function(data){
  
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
      legend.position = "blank",
      axis.title.y = element_blank()
    )
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                      clean Question 18b FULLY CORRECT                    ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for just one PLO assessment (pre or post)  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q18b_FULLY_CORRECT <- function(PLO_data_clean){
  
  PLO_data_clean |> 
    
    # select necessary cols ----
  select(prob_dist_terms) |> 
    
    # sum ----
  group_by(prob_dist_terms) |>
    count() |>
    ungroup() |> 
    
    # add correct or incorrect label ----
  mutate(correctness = case_when(
    prob_dist_terms == "normal" ~ "no",
    prob_dist_terms == "normal,bimodal,symmetric" ~ "no",
    prob_dist_terms == "normal,uniform" ~ "no",
    prob_dist_terms == "normal,uniform,bimodal" ~ "no",
    prob_dist_terms == "normal,uniform,bimodal,symmetric" ~ "yes",
    prob_dist_terms == "normal,uniform,bimodal,variable,symmetric" ~ "no"
  )) |> 
    
    # coerce data types ----
  mutate(correctness = as_factor(correctness)) |> 
    
    # sum ----
  group_by(correctness) |> 
    summarize(total = sum(n)) |> 
    ungroup() |> 
    
    # reorder factors ----
  # mutate(correctness = fct_relevel(correctness, c("Yes", "No", "NA")))
  
  # add col for percentages ----
  mutate(percentage = round((total/(sum(total)))*100, 1),
         perc_label = paste0(percentage, "%")) 
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for both pre & post assessments  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q18b_FULLY_CORRECT_bothPP <- function(PLO_data_clean){
  
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
##                      plot Question 18b FULLY CORRECT                     ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for just one PLO assessment (pre or post)  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q18b_FULLY_CORRECT <- function(data){
  
  ggplot(data, aes(x = fct_reorder(correctness, desc(total)), y = total, label = perc_label, fill = correctness)) +
    geom_col() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Did they get the question fully correct?",
         caption = "Question 18b") +
    scale_fill_manual(values = pal, limits = names(pal)) +
    meds_theme()
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for both pre & post assessments  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q18b_FULLY_CORRECT_bothPP <- function(data){
  
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
      legend.position = "blank",
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
  
}
  
