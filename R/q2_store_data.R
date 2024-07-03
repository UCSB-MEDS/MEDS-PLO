
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            clean Question 2 data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q2_store_data <- function(PLO_data_clean){
  
  PLO_data_clean |> 
    
  # select necessary cols ----
  select(where_store_data, where_store_data_6_text) |> 
    
  # split strings by `,` delim ----
  separate_longer_delim(where_store_data, delim = ",") |> 
    
  # if "Other", replace with written-in response ----
  mutate(store_data_new = case_when(
    where_store_data == "Other" ~ where_store_data_6_text,
    TRUE ~ where_store_data
  )) |>
    
  # remove unnecessary cols ----
  select(where_store_data = store_data_new) |> 
    
  # combine similar "other" choices ----
  mutate(where_store_data = case_when(
    where_store_data %in% server ~ "Server",
    TRUE ~ where_store_data
  )) |> 
    
  # change "Locally on my computer" to "Locally" ----
  mutate(across('where_store_data', str_replace, 'Locally on my computer', 'Locally')) |> 
    
  # count ----
  group_by(where_store_data) |> 
    count() |> 
    ungroup() |> 
    
  # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            plot Question 2 data                          ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q2_store_data <- function(q2_store_data_clean){
  
  ggplot(q2_store_data_clean, aes(x = fct_reorder(where_store_data, desc(n)), y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    geom_text(position = position_stack(vjust = 0.5), size = 4, color = "white", family = "nunito") +
    labs(x = "Data Storage Location", y = "Number of MEDS students",
         title = "Where do you store data and/or documents (select all that apply)",
         caption = "Question 2") +
    meds_theme()
  
}

