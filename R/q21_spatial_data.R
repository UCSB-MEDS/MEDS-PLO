
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------------- CLEAN QUESTION 21A DATA-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q21a_comfort_spatial <- function(PLO_data_clean){
  
  # ........................to iterate over.........................
  options <- c("1 (never worked with it before)", "2", "3", "4", "5 (work with it all the time)")
  
  #........................initial wrangling.......................
  df <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(spatial_data, timepoint) |> 
    
    # sum ----
  group_by(timepoint, spatial_data) |> 
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
      new_row <- data.frame(timepoint = "Pre-MEDS", spatial_data = cat_name, n = 0)
      pre_meds <- rbind(pre_meds, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  #.................calculate pre-MEDS percentages.................
  pre_meds <- pre_meds |> 
    mutate(total_respondents = sum(n),
           percentage = round((n/total_respondents)*100, 1),
           perc_label = paste0(percentage, "%")) |>
    mutate(xvar = spatial_data)
  
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
      new_row <- data.frame(timepoint = "Post-MEDS", spatial_data = cat_name, n = 0)
      post_meds <- rbind(post_meds, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  #................calculate post-MEDS percentages.................
  post_meds <- post_meds |> 
    mutate(total_respondents = sum(n),
           percentage = round((n/total_respondents)*100, 1),
           perc_label = paste0(percentage, "%")) |>
    mutate(xvar = spatial_data)
  
  ##~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ recombine dfs  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~
  
  all_q21a_spatial_data <- rbind(pre_meds, post_meds) |> 
    mutate(spatial_data = fct_relevel(spatial_data,
                                       c("1 (never worked with it before)", 
                                         "2", "3", "4", 
                                         "5 (work with it all the time)"))) 
  
  return(all_q21a_spatial_data)
  
}
  
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##-------------------------- PLOT QUESTION 21A DATA-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# see `plot_rank_data.R`

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------------- CLEAN QUESTION 21B DATA-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q21b_rep_spatial <- function(PLO_data_clean){
  
  # ........................to iterate over.........................
  options <- c("vector", "raster", "relational", "tabular", "I'm not sure", NA)
  
  #........................initial wrangling.......................
  df <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(rep_spatial_data, timepoint) |> 
    
    # split strings by `,` delim ----
  separate_longer_delim(rep_spatial_data, delim = ",") |> 
    
    # sum ----
  group_by(timepoint, rep_spatial_data) |> 
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
      new_row <- data.frame(timepoint = "Pre-MEDS", rep_spatial_data = cat_name, n = 0)
      pre_meds <- rbind(pre_meds, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  #....total respondents that continued onto answer question 18b...
  # total_pre_resp <- meds2024_before_clean |> 
  #   select(rep_spatial_data) |>
  #   group_by(rep_spatial_data) |>
  #   filter(rep_spatial_data != "1 (never heard of it)") |>
  #   count() |>
  #   ungroup() |>
  #   summarize(n = sum(n)) |>
  #   pull()
  
  #.................calculate pre-MEDS percentages.................
  pre_meds <- pre_meds |> 
    mutate(total_respondents = pre_meds_num_respondents, # variable from index.qmd
           percentage = round((n/total_respondents)*100, 1),
           perc_label = paste0(percentage, "%")) |>
    mutate(xvar = rep_spatial_data)
  
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
      new_row <- data.frame(timepoint = "Post-MEDS", rep_spatial_data = cat_name, n = 0)
      post_meds <- rbind(post_meds, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  # #....total respondents that continued onto answer question 18b...
  # total_post_resp <- meds2024_after_clean |>
  #   select(rep_spatial_data) |>
  #   group_by(rep_spatial_data) |>
  #   filter(rep_spatial_data != "1 (never heard of it)") |>
  #   count() |>
  #   ungroup() |>
  #   summarize(n = sum(n)) |>
  #   pull()
  
  #................calculate post-MEDS percentages.................
  post_meds <- post_meds |> 
    mutate(total_respondents = post_meds_num_respondents, # variable from `index.qmd`
           percentage = round((n/total_respondents)*100, 1),
           perc_label = paste0(percentage, "%")) |>
    mutate(xvar = rep_spatial_data)
  
  ##~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ recombine dfs  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~
  
  all_q21b_rep_spatial_data <- rbind(pre_meds, post_meds) |> 
    drop_na() |> 
    mutate(xvar = fct_relevel(xvar,
                              c("vector", "raster", "relational", "tabular", "I'm not sure"))) 
  
  return(all_q21b_rep_spatial_data)
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##-------------------------- PLOT QUESTION 21B DATA-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q21b_rep_spatial <- function(data){
  
  ggplot(na.omit(data), aes(x = fct_relevel(xvar,
                                            c("I'm not sure", "tabular", "relational", "raster", "vector")), 
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
         title = "What are the two primary ways of representing spatial data (select\ntwo)?",
         subtitle = "Correct answer: vector, raster",
         caption = "Question 21b") +
    facet_wrap(~timepoint) +
    meds_theme() +
    theme(
      legend.position = "blank",
      axis.title.y = element_blank()
    )
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##-------------------- CLEAN QUESTION 21B - FULLY CORRECT-----------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q21b_FULLY_CORRECT <- function(PLO_data_clean){
  
  df <- PLO_data_clean |> 
    
    # select necessary cols ----
    select(rep_spatial_data, timepoint) |> 
    
    # sum ----
    group_by(timepoint, rep_spatial_data) |>
    
    mutate(correctness = ifelse(rep_spatial_data == "raster,vector", 
                                yes = "yes",
                                no = "no")) 
  
  ##~~~~~~~~~~~~~~~~~~
  ##  ~ pre-MEDS  ----
  ##~~~~~~~~~~~~~~~~~~
  
  #....total respondents that continued onto answer question 18b...
  total_pre_resp <- meds2024_before_clean |> 
    select(rep_spatial_data) |>
    group_by(rep_spatial_data) |>
    filter(rep_spatial_data != "1 (never worked with it before)") |>
    count() |>
    ungroup() |>
    summarize(n = sum(n)) |>
    pull()
  
  pre_meds <- df |> 
    filter(timepoint == "Pre-MEDS") |> 
    group_by(correctness) |> 
    count() |> 
    ungroup() |> 
    mutate(timepoint = rep("Pre-MEDS")) |> 
    mutate(total_respondents = sum(n), #total_pre_resp,
           percentage = round((n/total_respondents)*100, 1),
           perc_label = paste0(percentage, "%"))
  
  ##~~~~~~~~~~~~~~~~~~~
  ##  ~ post-MEDS  ----
  ##~~~~~~~~~~~~~~~~~~~
  
  #....total respondents that continued onto answer question 18b...
  total_post_resp <- meds2024_after_clean |> 
    select(rep_spatial_data) |>
    group_by(rep_spatial_data) |>
    filter(rep_spatial_data != "1 (never worked with it before)") |>
    count() |>
    ungroup() |>
    summarize(n = sum(n)) |>
    pull()
  
  post_meds <- df |> 
    filter(timepoint == "Post-MEDS") |> 
    group_by(correctness) |> 
    count() |> 
    ungroup() |> 
    mutate(timepoint = rep("Post-MEDS")) |> 
    mutate(total_respondents = sum(n), #total_post_resp,
           percentage = round((n/total_respondents)*100, 1),
           perc_label = paste0(percentage, "%"))
  
  ##~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ recombine dfs  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~
  
  all_q21b_fully_correct <- rbind(pre_meds, post_meds) |> 
    filter(correctness == "yes") |> 
    mutate(timepoint = fct_relevel(timepoint, c("Pre-MEDS", "Post-MEDS"))) |> 
    mutate(perc_label_long = paste0(perc_label, "\n(", n, "/", total_respondents, " respondents)"))
  
  return(all_q21b_fully_correct)
  
}
  
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------- PLOT QUESTION 21B FULLY CORRECT-------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q21b_FULLY_CORRECT <- function(data){
  
  ggplot(data, aes(x = timepoint, y = percentage)) +
    geom_col(aes(fill = timepoint), position = position_dodge(preserve = "total")) +
    geom_text(aes(label = perc_label_long), 
              position = position_stack(vjust = 0.5), 
              size = 3, color = "white", family = "nunito") +
    labs(title = "% of respondents who correctly answered question 21b",
         subtitle = "(i.e. chose exactly the following options: raster, vector)") +
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

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------------- CLEAN QUESTION 21C DATA-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q21c_vec_ras <- function(PLO_data_clean){
  
  #........................initial wrangling.......................
  df <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(vec_or_ras, timepoint) |>
    
    # sum ----
  group_by(timepoint, vec_or_ras) |>
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
    mutate(xvar = vec_or_ras) |> 
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
    mutate(xvar = vec_or_ras) |> 
    mutate(perc_label_long = paste0(perc_label, "\n(", n, "/", total_respondents, " respondents)"))
  
  #~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ recombine dfs  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~
  
  all_q21c_data <- rbind(pre_meds, post_meds) |> 
    filter(vec_or_ras == "vector") 
  
  return(all_q21c_data)
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##-------------------------- PLOT QUESTION 21C DATA-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# see `plot_correct_answer_comparison.R`
  