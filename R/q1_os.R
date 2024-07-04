
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 1 data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for just one PLO assessment (pre or post)  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q1_os <- function(PLO_data_clean){
  
  PLO_data_clean |> 
    
  # select necessary cols ----
  select(operating_system, operating_system_4_text) |> 
    
  # if "Other", replace with written-in response ----
  mutate(os = case_when(
    operating_system == "Other" ~ operating_system_4_text,
    TRUE ~ operating_system
  )) |> 
    
  # remove unnecessary cols ----
  select(os) |> 
    
  # count ----
  group_by(os) |> 
    count() |> 
    ungroup() |> 
    
  # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for both pre & post assessments  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q1_os_bothPP <- function(PLO_data_clean){
 
  #........................initial wrangling.......................
  df <- PLO_data_clean |> 
    
  # select necessary cols ----
  select(operating_system, operating_system_4_text, timepoint) |> 
    
  # if "Other", replace with written-in response ----
  mutate(os = case_when(
    operating_system == "Other" ~ operating_system_4_text,
    TRUE ~ operating_system
  )) |> 
    
  # remove unnecessary cols ----
  select(os, timepoint) |> 
    
  # count ----
  group_by(timepoint, os) |> 
    count() |> 
    ungroup() #|> 
  
  #........separate pre-MEDS data to calculate percentages.........
  pre_meds <- df |> 
    filter(timepoint == "Pre-MEDS") |> 
    mutate(total_respondents = sum(n),
           percentage = round((n/(sum(n)))*100, 1),
           perc_label = paste0(percentage, "%"))
  
  #........separate post-MEDS data to calculate percentages........
  post_meds <- df |> 
    filter(timepoint == "Post-MEDS") |> 
    mutate(total_respondents = sum(n),
           percentage = round((n/(sum(n)))*100, 1),
           perc_label = paste0(percentage, "%"))
    
  #..........................recombine dfs.........................
  all_q1_data <- rbind(pre_meds, post_meds)
  
  #..........................return data...........................
  return(all_q1_data)
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            plot Question 1 data                          ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for just one PLO assessment (pre or post)  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q1_os <- function(q1_data_clean){
  
  ggplot(q1_data_clean, aes(x = fct_reorder(os, desc(n)), y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    geom_text(position = position_stack(vjust = 0.5), size = 4, color = "white", family = "nunito") +
    labs(x = "Operating System", y = "Number of MEDS students",
         title = "What operating system is on the computer you are using?",
         caption = "Question 1") +
    meds_theme()
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ for both pre & post assessments  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q1_os_bothPP <- function(q1_data_clean){
  
  ggplot(q1_data_clean, aes(x = fct_reorder(os, desc(n)), y = n, label = perc_label)) +
    geom_col(aes(fill = timepoint), color = "white",
             position = position_dodge()) +
    geom_text(position = position_dodge2(width = 0.9), vjust = 2, 
              size = 4, color = "white", family = "nunito") +
    scale_fill_manual(values = meds_pal) +
    labs(x = "Operating System", y = "Number of MEDS students",
         title = "What operating system is on the computer you are using?",
         caption = "Question 1") +
    meds_theme()

}
