
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 1 data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

q1_os_clean <- function(PLO_data_clean){
  
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


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            plot Question 1 data                          ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

q1_os_plot <- function(q1_data_clean){
  
  ggplot(q1_data_clean, aes(x = fct_reorder(os, desc(n)), y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    geom_text(position = position_stack(vjust = 0.5), size = 4, color = "white", family = "nunito") +
    labs(x = "Operating System", y = "Number of MEDS students",
         title = "What operating system is on the computer you are using?",
         caption = "Question 1") +
    meds_theme
  
}