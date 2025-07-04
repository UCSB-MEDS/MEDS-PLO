
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------------- CLEAN QUESTION 2 DATA-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q2_store_data <- function(PLO_data_clean){
  
  #........................initial wrangling.......................
  df <- PLO_data_clean |> 
    
    # select necessary cols ----
    select(where_store_data, where_store_data_6_text, timepoint) |> 
    
    # split strings by `,` delim ----
    separate_longer_delim(where_store_data, delim = ",") |> 
    
    # if "Other", replace with written-in response ----
    mutate(store_data_new = case_when(
      where_store_data == "Other" ~ where_store_data_6_text,
      TRUE ~ where_store_data
    )) |>
    
    # remove unnecessary cols ----
    select(where_store_data = store_data_new, timepoint) |> 
    
    # split strings by `;` delim (someone had written in 'Other' and separated responses with ;) ----    
    separate_longer_delim(where_store_data, delim = "; ") |> 
    
    # make consistent responses ----
    mutate(where_store_data = case_when(
      where_store_data %in% server ~ "Server",
      where_store_data %in% external_drive ~ "External Hard Drive",
      where_store_data %in% icloud ~ "iCloud",
      where_store_data %in% github ~ "GitHub",
      where_store_data %in% locally ~ "Locally",
      TRUE ~ where_store_data
    )) |> 
    
    # make consistent responses ----
    # mutate(where_store_data = str_replace(string = where_store_data,
    #                                       pattern = "Locally on my computer", 
    #                                       replacement = "Locally"),
    #       where_store_data = str_replace(string = where_store_data, 
    #                                       pattern = "Github", 
    #                                       replacement = "GitHub")) |>
    
    # count ----
    group_by(timepoint, where_store_data) |> 
      count() |> 
      ungroup() 
  
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
  all_q2_data <- rbind(pre_meds, post_meds)
  
  #..........................return data...........................
  return(all_q2_data)
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------------- PLOT QUESTION 2 DATA------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_q2_store_data <- function(q2_store_data_clean){
  
  ggplot(na.omit(q2_store_data_clean), aes(x = fct_reorder(where_store_data, n), y = n)) + 
    geom_col(aes(fill = timepoint), position = position_dodge(preserve = "total")) +
    coord_flip() + 
    geom_text( # see: https://cedricscherer.com/2023/10/26/yet-another-how-to-on-labelling-bar-graphs-in-ggplot2/
      aes(label = paste0("  ", sprintf("%2.1f", percentage), "%  "), 
          hjust = percentage > 3),
      position = position_dodge2(width = 0.9),
      size = 3, color = "white", family = "nunito"
    ) +
    scale_fill_manual(values = meds_pal) +
    labs(x = "Data Storage Location", y = "Number of MEDS students",
         title = "Where do you store data and/or documents (select all that apply)",
         caption = "Question 2") +
    facet_wrap(~timepoint) +
    meds_theme() +
    theme(
      legend.position = "blank",
      axis.title.y = element_blank()
    )
  
}
