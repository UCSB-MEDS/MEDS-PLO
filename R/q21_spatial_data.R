##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 21a data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q21a_comfort_spatial <- function(PLO_data_clean){
  
  # to iterate over ----
  options <- c("1 (never worked with it before)", "2", "3", "4", "5 (work with it all the time)")
  
  df1 <- PLO_data_clean |> 
    
  # select necessary cols ----
  select(spatial_data) |> 
    
  # sum ----
  group_by(spatial_data) |>
    count() |>
    ungroup() 
    
  # ADDING BC NO ONE SELECTED THE FOLLOWING OPTIONS ----
  # add_row(spatial_data = "1 (never worked with it before)", n = 0) |>
  for (i in 1:length(options)){
    
    cat_name <- options[i]
    
    # if category already exists in df, skip to next one
    if (cat_name %in% pull(df1[,1])) {
      
      message(cat_name, " already exists. Moving to next option.")
      df1 <- df1
      
      # if category doesn't already exist, add it with n = 0 so that it still shows up on plot
    } else {
      
      message(cat_name, " does not exist. Adding now.")
      new_row <- data.frame(spatial_data = cat_name, n = 0)
      df1 <- rbind(df1, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  # finish wrangling ----
  df2 <- df1 |> 
  
  # reorder factors ----
  mutate(spatial_data = fct_relevel(spatial_data, 
                                    c("1 (never worked with it before)", "2", 
                                      "3", "4", "5 (work with it all the time)"))) |>
    
  # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           plot Question 21a data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q21a_comfort_spatial <- function(data){
  
  ggplot(data, aes(x = spatial_data, y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    coord_flip() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Comfort level",
         title = "How comfortable are you working with spatial data?",
         caption = "Question 21a (choosing '1 (never worked with it before)' skips respondent to question 22)") +
    meds_theme()
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 21b data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q21b_rep_spatial <- function(PLO_data_clean){
  
  PLO_data_clean |>
    
  # select necessary cols ----
  select(rep_spatial_data) |>
    
  # split strings by `,` delim ----
  separate_longer_delim(rep_spatial_data, delim = ",") |>
    
  # sum ----
  group_by(rep_spatial_data) |>
    count() |>
    ungroup() |>
    
  # ADDING BC NO ONE SELECTED THE FOLLOWING OPTIONS ----
  add_row(rep_spatial_data = "tabular", n = 0) |>
    add_row(rep_spatial_data = "relational", n = 0) |>
    
  # add col for percentages ----
  mutate(percentage = round((n/q21b_num_answers)*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           plot Question 21b data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q21b_rep_spatial <- function(data){
  
  ggplot(data, aes(x = fct_reorder(rep_spatial_data, desc(n)), y = n, label = perc_label, fill = rep_spatial_data)) +
    geom_col() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Selection",
         title = "What are the two primary ways of representing spatial data (select\ntwo)?",
         caption = "Question 21b") +
    scale_fill_manual(values = pal, limits = names(pal)) +
    meds_theme()
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                      clean Question 21b FULLY CORRECT                    ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q21b_FULLY_CORRECT <- function(PLO_data_clean){
  
  PLO_data_clean |> 
    
  # select necessary cols ----
  select(rep_spatial_data) |> 
  
  # code correct responses as 'yes'  
  mutate(correctness = case_when(
      rep_spatial_data == "raster,vector" ~ "yes",
    )) |> 
    
  # sum ----
  group_by(correctness) |>
    count() |>
    ungroup() |> 
    
  # add no for plotting ----
  add_row(correctness = "no", n = 0) |>
    
  # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                      plot Question 21b FULLY CORRECT                     ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q21b_FULLY_CORRECT <- function(data){
  
  ggplot(data, aes(x = fct_reorder(correctness, desc(n)), y = n, label = perc_label, fill = correctness)) +
    geom_col() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Did they get the question fully correct?",
         caption = "Question 21b") +
    scale_fill_manual(values = pal, limits = names(pal)) +
    meds_theme()
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 21c data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q21c_vec_ras <- function(PLO_data_clean){
  
  PLO_data_clean |> 
    
  # select necessary cols ----
  select(vec_or_ras) |> 
    
  # sum ----
  group_by(vec_or_ras) |>
    count() |>
    ungroup() |> 
    
  # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           plot Question 21c data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q21c_vec_ras <- function(data){
  
  ggplot(data, aes(x = fct_reorder(vec_or_ras, desc(n)), y = n, label = perc_label, fill = vec_or_ras)) +
    geom_col() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Selection",
         title = "Is the following a vector or raster?",
         caption = "Question 21c") +
    scale_fill_manual(values = pal, limits = names(pal)) +
    meds_theme()
  
}

