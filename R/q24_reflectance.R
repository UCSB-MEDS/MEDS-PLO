
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------------- CLEAN QUESTION 24A DATA-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q24a_familiarity_rs <- function(PLO_data_clean){
  
  # ........................to iterate over.........................
  options <- c("1 (never heard of it)", "2", "3 (vague sense of what it means)", "4", "5 (very familiar)")
  
  #........................initial wrangling.......................
  df <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(reflec_spec, timepoint) |> 
    
    # sum ----
  group_by(timepoint, reflec_spec) |> 
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
      new_row <- data.frame(timepoint = "Pre-MEDS", reflec_spec = cat_name, n = 0)
      pre_meds <- rbind(pre_meds, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  #.................calculate pre-MEDS percentages.................
  pre_meds <- pre_meds |> 
    mutate(total_respondents = sum(n),
           percentage = round((n/total_respondents)*100, 1),
           perc_label = paste0(percentage, "%")) |>
    mutate(xvar = reflec_spec)
  
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
      new_row <- data.frame(timepoint = "Post-MEDS", reflec_spec = cat_name, n = 0)
      post_meds <- rbind(post_meds, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  #................calculate post-MEDS percentages.................
  post_meds <- post_meds |> 
    mutate(total_respondents = sum(n),
           percentage = round((n/total_respondents)*100, 1),
           perc_label = paste0(percentage, "%")) |>
    mutate(xvar = reflec_spec)
  
  ##~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ recombine dfs  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~
  
  all_q24a_reflec_spec <- rbind(pre_meds, post_meds) |> 
    mutate(reflec_spec = fct_relevel(reflec_spec,
                                          c("1 (never heard of it)", 
                                            "2", "3 (vague sense of what it means)", 
                                            "4", "5 (very familiar)")))
  
  
  return(all_q24a_reflec_spec)
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##-------------------------- PLOT QUESTION 24A DATA-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# see `plot_rank_data.R`

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------------- CLEAN QUESTION 24B DATA-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q24b_veg_wave <- function(PLO_data_clean){
  
  #........................initial wrangling.......................
  df <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(veg_wavelength, timepoint) |>
    
    # sum ----
  group_by(timepoint, veg_wavelength) |>
    count() |>
    ungroup() 
  
  ##~~~~~~~~~~~~~~~~~~
  ##  ~ pre-MEDS  ----
  ##~~~~~~~~~~~~~~~~~~
  
  #.........separate pre-MEDS (to add 0s for missing cats).........
  pre_meds <- df |> 
    filter(timepoint == "Pre-MEDS") |> 
    #drop_na() |> # include this if you want % calculation to be out of only students who advanced to this question
    mutate(total_respondents = sum(n),
           percentage = round((n/total_respondents)*100, 1),
           perc_label = paste0(percentage, "%")) |>
    mutate(xvar = veg_wavelength) |> 
    mutate(perc_label_long = paste0(perc_label, "(", n, "/", total_respondents, " respondents)"))
  
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
    mutate(xvar = veg_wavelength) |> 
    mutate(perc_label_long = paste0(perc_label, "\n(", n, "/", total_respondents, " respondents)"))
  
  #~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ recombine dfs  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~
  
  all_q24b_data <- rbind(pre_meds, post_meds) |> 
    filter(veg_wavelength == "green") 
  
  return(all_q24b_data)
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##-------------------------- PLOT QUESTION 24B DATA-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# see `plot_correct_answer_comparison.R`
