clean_rank_data <- function(all_PLO_data, col_name){
  
  #........................to iterate over.........................
  options <- c("1 (strongly disagree)", "2", "3 (neutral)", "4", "5 (strongly agree)")
  
  #........................to iterate over.........................
  df <- PLO_data_clean |> 
    select(!!sym(col_name), timepoint) |>
    group_by(timepoint, !!sym(col_name)) |>
    count() |> 
    ungroup() 
  
  #................separate pre- and post-MEDS data................
  pre_meds <- df |>
    filter(timepoint == "Pre-MEDS")
  post_meds <- df |>
    filter(timepoint == "Post-MEDS")
  
  #......................add 0s where missing......................
  for (i in 1:length(options)){
    cat_name <- options[i]
    
    # ---- Check for this category in the Pre-Meds df ----
    # if category already exists in df, skip to next one ----
    if (cat_name %in% pull(pre_meds[,2])) {
      message(cat_name, " already exists in Pre-Meds. Moving to next option.")
      pre_meds <- pre_meds
      # if category doesn't already exist, add it with n = 0 so that it still shows up on plot ----
    } else {
      message(cat_name, " does not exist in Pre-Meds. Adding now.")
      new_row <- tibble(timepoint = "Pre-MEDS", !!col_name := cat_name, n = 0)
      pre_meds <- rbind(pre_meds, new_row)
      
    }
    # ---- Check for this category in the Post-Meds df ----
    # if category already exists in df, skip to next one ----
    if (cat_name %in% pull(post_meds[,2])) {
      message(cat_name, " already exists in Post-Meds. Moving to next option.")
      post_meds <- post_meds
      # if category doesn't already exist, add it with n = 0 so that it still shows up on plot ----  
    } else {
      message(cat_name, " does not exist in Post-Meds. Adding now.")
      new_row <- tibble(timepoint = "Post-MEDS", !!col_name := cat_name, n = 0)
      post_meds <- rbind(post_meds, new_row)
    }
    
    message("----------------------")
    
  }
  
  # ##~~~~~~~~~~~~~~~~~~
  # ##  ~ pre-MEDS  ----
  # ##~~~~~~~~~~~~~~~~~~
  # 
  # #.........separate pre-MEDS (to add 0s for missing cats).........
  # pre_meds <- df |> 
  #   filter(timepoint == "Pre-MEDS") 
  
  # #................add 0s where missing (pre-MEDS).................
  # for (i in 1:length(options)){
  #   
  #   cat_name <- options[i]
  #   
  #   # if category already exists in df, skip to next one ----
  #   if (cat_name %in% pull(pre_meds[,2])) {
  #     
  #     message(cat_name, " already exists. Moving to next option.")
  #     pre_meds <- pre_meds
  #     
  #     # if category doesn't already exist, add it with n = 0 so that it still shows up on plot ----
  #   } else {
  #     
  #     message(cat_name, " does not exist. Adding now.")
  #     new_row <- data.frame(timepoint = "Pre-MEDS", raw_data = cat_name, n = 0)
  #     pre_meds <- rbind(pre_meds, new_row)
  #     
  #   }
  #   
  #   message("----------------------")
  #   
  # } 
  
  #.................calculate pre-MEDS percentages.................
  pre_meds <- pre_meds |> 
    mutate(total_respondents = sum(n),
           percentage = round((n/total_respondents)*100, 1),
           perc_label = paste0(percentage, "%")) |>
    mutate(xvar = raw_data)
  
  # ##~~~~~~~~~~~~~~~~~~~
  # ##  ~ post-MEDS  ----
  # ##~~~~~~~~~~~~~~~~~~~
  # 
  # #........separate post-MEDS (to add 0s for missing cats).........
  # post_meds <- df |> 
  #   filter(timepoint == "Post-MEDS") 
  
  # #................add 0s where missing (post-MEDS)................
  # for (i in 1:length(options)){
  #   
  #   cat_name <- options[i]
  #   
  #   # if category already exists in df, skip to next one ----
  #   if (cat_name %in% pull(post_meds[,2])) {
  #     
  #     message(cat_name, " already exists. Moving to next option.")
  #     post_meds <- post_meds
  #     
  #     # if category doesn't already exist, add it with n = 0 so that it still shows up on plot ----
  #   } else {
  #     
  #     message(cat_name, " does not exist. Adding now.")
  #     new_row <- data.frame(timepoint = "Post-MEDS", raw_data = cat_name, n = 0)
  #     post_meds <- rbind(post_meds, new_row)
  #     
  #   }
  #   
  #   message("----------------------")
  #   
  # } 
  # 
  #................calculate post-MEDS percentages.................
  post_meds <- post_meds |> 
    mutate(total_respondents = sum(n),
           percentage = round((n/total_respondents)*100, 1),
           perc_label = paste0(percentage, "%")) |>
    mutate(xvar = raw_data)
  
  ##~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ recombine dfs  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~
  
  all_q9_data <- rbind(pre_meds, post_meds) |> 
    mutate(raw_data = fct_relevel(raw_data,
                                  c("1 (strongly disagree)", "2", 
                                    "3 (neutral)", "4", "5 (strongly agree)"))) 
  
  return(all_q9_data)
  
}