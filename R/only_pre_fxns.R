
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##----------------------------- QUESTION 1 (OS)---------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q1_os_pre <- function(PLO_data_clean){
  
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

plot_q1_os_pre <- function(q1_data_clean){
  
  ggplot(q1_data_clean, aes(x = fct_reorder(os, desc(n)), y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    geom_text(position = position_stack(vjust = 0.5), size = 4, color = "white", family = "nunito") +
    labs(x = "Operating System", y = "Number of MEDS students",
         title = "What operating system is on the computer you are using?",
         caption = "Question 1") +
    meds_theme()
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------------- QUESTION 2 (DATA STORAGE)---------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q2_store_data_pre <- function(PLO_data_clean){
  
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
    
    # make consistent responses ----
  mutate(where_store_data = str_replace(string = where_store_data,
                                        pattern = "Locally on my computer", 
                                        replacement = "Locally"),
         where_store_data = str_replace(string = where_store_data, 
                                        pattern = "Github", 
                                        replacement = "GitHub")) |>
    
    # count ----
  group_by(where_store_data) |> 
    count() |> 
    ungroup() |> 
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

plot_q2_store_data_pre <- function(q2_store_data_clean, survey){
  
  if (survey == "Pre") {
    
    color <- "#047C91"
    
  } else if (survey == "Post") {
    
    color <- "#003660"
    
  }
  
  ggplot(na.omit(q2_store_data_clean), aes(x = fct_reorder(where_store_data, n), y = n, label = perc_label)) +
    geom_col(fill = color) +
    coord_flip() + 
    geom_text( # see: https://cedricscherer.com/2023/10/26/yet-another-how-to-on-labelling-bar-graphs-in-ggplot2/
      aes(label = paste0("  ", sprintf("%2.1f", percentage), "%  "), 
          hjust = percentage > 5),
      position = position_dodge2(width = 0.9),
      size = 3, color = "white", family = "nunito"
    ) +
    #geom_text(position = position_stack(vjust = 0.5), size = 4, color = "white", family = "nunito") +
    labs(x = "Data Storage Location", y = "Number of MEDS students",
         title = "Where do you store data and/or documents (select all that apply)",
         caption = "Question 2") +
    meds_theme()
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##----------------------------- QUESTION 3 (GUI)--------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q3_gui_pre <- function(PLO_data_clean){
  
  # to iterate over ----
  options <- c("Never", "Less than once per year", "Several times per year", "Monthly", "Weekly", "Daily")
  
  # select var of interest ----
  df1 <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(point_and_click_gui) |> 
    
    # sum ----
  group_by(point_and_click_gui) |> 
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
      new_row <- data.frame(point_and_click_gui = cat_name, n = 0)
      df1 <- rbind(df1, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  # finish wrangling ----
  df2 <- df1 |> 
    
    # reorder factors ----
  mutate(point_and_click_gui = fct_relevel(point_and_click_gui,
                                           c("Never", "Less than once per year", "Several times per year",
                                             "Monthly", "Weekly", "Daily"))) |>
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%")) |>
    
    # create col with xvar name for plotting consistency ----
  mutate(xvar = point_and_click_gui)
  
  # return final wrangled df
  return(df2)
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------- QUESTION 4 (PROGRAMMING LANGUAGE)-----------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q4_prog_lang_pre <- function(PLO_data_clean){
  
  # to iterate over ----
  options <- c("Never", "Less than once per year", "Several times per year", "Monthly", "Weekly", "Daily")
  
  # select var of interest ----
  df1 <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(program_lang) |> 
    
    # sum ----
  group_by(program_lang) |> 
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
      new_row <- data.frame(program_lang = cat_name, n = 0)
      df1 <- rbind(df1, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  # finish wrangling ----
  df2 <- df1 |> 
    
    # reorder factors ----
  mutate(program_lang = fct_relevel(program_lang,
                                    c("Never", "Less than once per year", "Several times per year",
                                      "Monthly", "Weekly", "Daily"))) |>
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%")) |>
    
    # create col with xvar name for plotting consistency ----
  mutate(xvar = program_lang)
  
  # return final wrangled df
  return(df2)
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##-------------------------- QUESTION 5 (DATABASES)-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q5_databases_pre <- function(PLO_data_clean){
  
  # to iterate over ----
  options <- c("Never", "Less than once per year", "Several times per year", "Monthly", "Weekly", "Daily")
  
  # select var of interest ----
  df1 <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(databases) |> 
    
    # sum ----
  group_by(databases) |> 
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
      new_row <- data.frame(databases = cat_name, n = 0)
      df1 <- rbind(df1, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  # finish wrangling ----
  df2 <- df1 |> 
    
    # reorder factors ----
  mutate(databases = fct_relevel(databases,
                                 c("Never", "Less than once per year", "Several times per year",
                                   "Monthly", "Weekly", "Daily"))) |>
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%")) |>
    
    # create col with xvar name for plotting consistency ----
  mutate(xvar = databases)
  
  # return final wrangled df
  return(df2)
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##----------------------- QUESTION 6 (VERSION CONTROL)--------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q6_version_control_pre <- function(PLO_data_clean){
  
  # to iterate over ----
  options <- c("Never", "Less than once per year", "Several times per year", "Monthly", "Weekly", "Daily")
  
  # select var of interest ----
  df1 <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(version_control) |> 
    
    # sum ----
  group_by(version_control) |> 
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
      new_row <- data.frame(version_control = cat_name, n = 0)
      df1 <- rbind(df1, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  # finish wrangling ----
  df2 <- df1 |> 
    
    # reorder factors ----
  mutate(version_control = fct_relevel(version_control,
                                       c("Never", "Less than once per year", "Several times per year",
                                         "Monthly", "Weekly", "Daily"))) |>
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%")) |>
    
    # create col with xvar name for plotting consistency ----
  mutate(xvar = version_control)
  
  # return final wrangled df
  return(df2)
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------------ QUESTION 7 (COMMAND SHELL)---------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q7_command_shell_pre <- function(PLO_data_clean){
  
  # to iterate over ----
  options <- c("Never", "Less than once per year", "Several times per year", "Monthly", "Weekly", "Daily")
  
  # select var of interest ----
  df1 <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(command_shell) |> 
    
    # sum ----
  group_by(command_shell) |> 
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
      new_row <- data.frame(command_shell = cat_name, n = 0)
      df1 <- rbind(df1, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  # finish wrangling ----
  df2 <- df1 |> 
    
    # reorder factors ----
  mutate(command_shell = fct_relevel(command_shell,
                                     c("Never", "Less than once per year", "Several times per year",
                                       "Monthly", "Weekly", "Daily"))) |>
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%")) |>
    
    # create col with xvar name for plotting consistency ----
  mutate(xvar = command_shell)
  
  # return final wrangled df
  return(df2)
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------- QUESTION 3-7 (PLOT FREQUENCY USE)-----------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_frequency_use_pre <- function(data, survey, title, caption){
  
  if (survey == "Pre") {
    
    color <- "#047C91"
    
  } else if (survey == "Post") {
    
    color <- "#003660"
    
  }
  
  ggplot(data, aes(x = xvar, y = n, label = perc_label)) +
    geom_col(fill = color) +
    coord_flip() +
    geom_text(position = position_stack(vjust = 0.5), size = 4, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Frequency of use",
         title = title,
         caption = caption) +
    meds_theme()
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##-------------------- QUESTION 8 (WORKFLOW SATISFACTION)-----------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q8_workflow_satisfaction_pre <- function(PLO_data_clean){
  
  # to iterate over ----
  options <- c("Very Satisfied", "Satisfied", "Neutral", "Unsatisfied", "Very unsatisfied", "Not sure", "Never thought about this", "Not applicable")
  
  # select var of interest ----
  df1 <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(rate_satisfaction) |> 
    
    # sum ----
  group_by(rate_satisfaction) |> 
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
      new_row <- data.frame(rate_satisfaction = cat_name, n = 0)
      df1 <- rbind(df1, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  # finish wrangling ----
  df2 <- df1 |> 
    
    # reorder factors ----
  mutate(rate_satisfaction = fct_relevel(rate_satisfaction,
                                         c("Not applicable", "Never thought about this", "Not sure",
                                           "Very unsatisfied", "Unsatisfied", "Neutral",
                                           "Satisfied", "Very Satisfied"))) |>
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%")) 
  
  # return final wrangled df
  return(df2)
  
}

plot_q8_workflow_satisfaction_pre <- function(q8_workflow_satisfaction_clean){
  
  ggplot(q8_workflow_satisfaction_clean, aes(x = rate_satisfaction, y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    coord_flip() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Satisfaction level",
         title = "Please rate your level of satisfaction with your current data\nmanagement and analysis workflow (i.e. how you collect, organize,\nstore, and analyze your data).",
         caption = "Question 8") +
    meds_theme()
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------------- QUESTION 9 (RAW DATA)-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q9_raw_data_pre <- function(PLO_data_clean){
  
  # to iterate over ----
  options <- c("1 (strongly disagree)", "2", "3 (neutral)", "4", "5 (strongly agree)")
  
  # select var of interest ----
  df1 <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(raw_data) |> 
    
    # sum ----
  group_by(raw_data) |> 
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
      new_row <- data.frame(raw_data = cat_name, n = 0)
      df1 <- rbind(df1, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  # finish wrangling ----
  df2 <- df1 |> 
    
    # reorder factors ----
  mutate(raw_data = fct_relevel(raw_data,
                                c("1 (strongly disagree)", "2", 
                                  "3 (neutral)", "4", "5 (strongly agree)"))) |>
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%")) |> 
    
    # create col with xvar name for plotting consistency ----
  mutate(xvar = raw_data)
  
  # return final wrangled df
  return(df2)
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##----------------------- QUESTION 10 (SMALL PROGRAM)---------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q10_small_program <- function(PLO_data_clean){
  
  # to iterate over ----
  options <- c("1 (strongly disagree)", "2", "3 (neutral)", "4", "5 (strongly agree)")
  
  # select var of interest ----
  df1 <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(small_program) |> 
    
    # sum ----
  group_by(small_program) |> 
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
      new_row <- data.frame(small_program = cat_name, n = 0)
      df1 <- rbind(df1, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  # finish wrangling ----
  df2 <- df1 |> 
    
    # reorder factors ----
  mutate(small_program = fct_relevel(small_program,
                                     c("1 (strongly disagree)", "2", 
                                       "3 (neutral)", "4", "5 (strongly agree)"))) |>
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%")) |> 
    
    # create col with xvar name for plotting consistency ----
  mutate(xvar = small_program)
  
  # return final wrangled df
  return(df2)
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##---------------------- QUESTION 11 (FIND HELP ONLINE)-------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q11_find_help_online <- function(PLO_data_clean){
  
  # to iterate over ----
  options <- c("1 (strongly disagree)", "2", "3 (neutral)", "4", "5 (strongly agree)")
  
  # select var of interest ----
  df1 <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(find_help_online) |> 
    
    # sum ----
  group_by(find_help_online) |> 
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
      new_row <- data.frame(find_help_online = cat_name, n = 0)
      df1 <- rbind(df1, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  # finish wrangling ----
  df2 <- df1 |> 
    
    # reorder factors ----
  mutate(find_help_online = fct_relevel(find_help_online,
                                        c("1 (strongly disagree)", "2", 
                                          "3 (neutral)", "4", "5 (strongly agree)"))) |>
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%")) |> 
    
    # create col with xvar name for plotting consistency ----
  mutate(xvar = find_help_online)
  
  # return final wrangled df
  return(df2)
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------- QUESTION 12 (OVERCOMING PROBLEMS)-----------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q12_overcoming_problems <- function(PLO_data_clean){
  
  # to iterate over ----
  options <- c("1 (strongly disagree)", "2", "3 (neutral)", "4", "5 (strongly agree)")
  
  # select var of interest ----
  df1 <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(overcoming_problems) |> 
    
    # sum ----
  group_by(overcoming_problems) |> 
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
      new_row <- data.frame(overcoming_problems = cat_name, n = 0)
      df1 <- rbind(df1, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  # finish wrangling ----
  df2 <- df1 |> 
    
    # reorder factors ----
  mutate(overcoming_problems = fct_relevel(overcoming_problems,
                                           c("1 (strongly disagree)", "2", 
                                             "3 (neutral)", "4", "5 (strongly agree)"))) |>
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%")) |> 
    
    # create col with xvar name for plotting consistency ----
  mutate(xvar = overcoming_problems)
  
  # return final wrangled df
  return(df2)
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------------- QUESTION 13 (CONFIDENCE)----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q13_confidence <- function(PLO_data_clean){
  
  # to iterate over ----
  options <- c("1 (strongly disagree)", "2", "3 (neutral)", "4", "5 (strongly agree)")
  
  # select var of interest ----
  df1 <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(confident_programmer) |> 
    
    # sum ----
  group_by(confident_programmer) |> 
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
      new_row <- data.frame(confident_programmer = cat_name, n = 0)
      df1 <- rbind(df1, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  # finish wrangling ----
  df2 <- df1 |> 
    
    # reorder factors ----
  mutate(confident_programmer = fct_relevel(confident_programmer,
                                            c("1 (strongly disagree)", "2", 
                                              "3 (neutral)", "4", "5 (strongly agree)"))) |>
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%")) |> 
    
    # create col with xvar name for plotting consistency ----
  mutate(xvar = confident_programmer)
  
  # return final wrangled df
  return(df2)
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##----------------------- QUESTION 14 (EASIER ANALYSIS)-------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q14_easier_analysis <- function(PLO_data_clean){
  
  # to iterate over ----
  options <- c("1 (strongly disagree)", "2", "3 (neutral)", "4", "5 (strongly agree)")
  
  # select var of interest ----
  df1 <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(easier_analyses) |> 
    
    # sum ----
  group_by(easier_analyses) |> 
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
      new_row <- data.frame(easier_analyses = cat_name, n = 0)
      df1 <- rbind(df1, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  # finish wrangling ----
  df2 <- df1 |> 
    
    # reorder factors ----
  mutate(easier_analyses = fct_relevel(easier_analyses,
                                       c("1 (strongly disagree)", "2", 
                                         "3 (neutral)", "4", "5 (strongly agree)"))) |>
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%")) |> 
    
    # create col with xvar name for plotting consistency ----
  mutate(xvar = easier_analyses)
  
  # return final wrangled df
  return(df2)
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------- QUESTION 15 (INCREASE EFFICIENCY)-----------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q15_increase_efficiency <- function(PLO_data_clean){
  
  # to iterate over ----
  options <- c("1 (strongly disagree)", "2", "3 (neutral)", "4", "5 (strongly agree)")
  
  # select var of interest ----
  df1 <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(increase_efficiency) |> 
    
    # sum ----
  group_by(increase_efficiency) |> 
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
      new_row <- data.frame(increase_efficiency = cat_name, n = 0)
      df1 <- rbind(df1, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  # finish wrangling ----
  df2 <- df1 |> 
    
    # reorder factors ----
  mutate(increase_efficiency = fct_relevel(increase_efficiency,
                                           c("1 (strongly disagree)", "2", 
                                             "3 (neutral)", "4", "5 (strongly agree)"))) |>
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%")) |> 
    
    # create col with xvar name for plotting consistency ----
  mutate(xvar = increase_efficiency)
  
  # return final wrangled df
  return(df2)
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##---------------------- QUESTION 9-15 (RANK AGREEMENT)-------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_rank_data_pre <- function(data, title, caption){
  
  ggplot(data, aes(x = xvar, y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    coord_flip() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Agreement level",
         title = title,
         caption = caption) +
    meds_theme()
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------------- QUESTION 16A (MEDIAN)-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q16a_median_pre <- function(PLO_data_clean){
  
  PLO_data_clean |> 
    
    # select necessary cols ----
  select(median) |> 
    
    # coerce to factor ----
  mutate(median = as_factor(median)) |> 
    
    # sum ----
  group_by(median) |>
    count() |>
    ungroup() |> 
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

plot_q16a_median_pre <- function(data){
  
  ggplot(data, aes(x = fct_reorder(median, desc(n)), y = n, label = perc_label, fill = median)) +
    geom_col() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Selection",
         title = "Calculate the median of this sample distribution: 5, 17, 0, 14, 14",
         subtitle = "Correct answer: 14",
         caption = "Question 16a (free response)") +
    scale_fill_manual(values = pal, limits = names(pal)) +
    meds_theme() +
    theme(
      legend.position = "none",
      plot.subtitle = element_text(face = "bold")
    )
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------------- QUESTION 16B (MODE)-------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q16b_mode_pre <- function(PLO_data_clean){
  
  PLO_data_clean |> 
    
    # select necessary cols ----
  select(mode) |> 
    
    # coerce to factor ----
  mutate(mode = as_factor(mode)) |> 
    
    # sum ----
  group_by(mode) |>
    count() |>
    ungroup() |> 
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

plot_q16b_mode_pre <- function(data){
  
  ggplot(data, aes(x = fct_reorder(mode, desc(n)), y = n, label = perc_label, fill = mode)) +
    geom_col() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Selection",
         title = "Calculate the mode of this sample distribution: 5, 17, 0, 14, 14",
         caption = "Question 16b (free response)") +
    scale_fill_manual(values = pal, limits = names(pal)) +
    meds_theme() +
    theme(
      legend.position = "none"
    )
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------- QUESTION 17A (FAMILIARITY WITH LINEAR REGRESSION)---------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q17a_familiar_lr_pre <- function(PLO_data_clean){
  
  # to iterate over ----
  options <- c("1 - never heard of it", "2", "3 - vague sense of what it means", "4", "5 - very familiar")
  
  df1 <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(linear_regression) |> 
    
    # sum ----
  group_by(linear_regression) |>
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
      new_row <- data.frame(linear_regression = cat_name, n = 0)
      df1 <- rbind(df1, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  # # ADDING BC NO ONE SELECTED THE FOLLOWING OPTIONS ----
  # add_row(linear_regression = "2", n = 0) |>
  # add_row(linear_regression = "1 - never heard of it", n = 0) |>
  
  # finish wrangling ----
  df2 <- df1 |> 
    
    # reorder factors ----
  mutate(linear_regression = fct_relevel(linear_regression, 
                                         c("1 - never heard of it", "2", 
                                           "3 - vague sense of what it means", "4", "5 - very familiar"))) |>
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

plot_q17a_familiar_lr_pre <- function(data){
  
  ggplot(data, aes(x = linear_regression, y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    coord_flip() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Familiarity level",
         title = "How familiar are you with the term linear regression?",
         caption = "Question 17a (choosing '1 - never heard of it' skips respondent to question 18)") +
    meds_theme()
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##----------------------- QUESTION 17B (MICROPLASTICS)--------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q17b_microplastics_pre <- function(PLO_data_clean){
  
  PLO_data_clean |> 
    
    # select necessary cols ----
  select(microplastics_lr) |> 
    
    # round values and remove sentences for plotting purposes ----
  mutate_if(is.character, str_replace_all, 
            pattern = "For 45 days of rain per year, we expect 27 pieces of microplastic, ceteris paribus.", replacement = "27") |> 
    
    # class 2023 after ----
  mutate_if(is.character, str_replace_all, pattern = "26.52", replacement = "27") |> 
    mutate_if(is.character, str_replace_all, pattern = "46.76823", replacement = "47") |> 
    mutate_if(is.character, str_replace_all, pattern = "46.77", replacement = "47") |> 
    mutate_if(is.character, str_replace_all, pattern = "900.28", replacement = "900") |> 
    mutate_if(is.character, str_replace_all, pattern = "47 mg", replacement = "47") |> 
    mutate_if(is.character, str_replace_all, pattern = "27 pieces", replacement = "27") |> 
    # class 2024 before ----
  mutate_if(is.character, str_replace_all, pattern = "Unsure", replacement = "I don't know") |> 
    mutate_if(is.character, str_replace_all, pattern = "i dont know", replacement = "I don't know") |> 
    mutate_if(is.character, str_replace_all, pattern = "no idea", replacement = "I don't know") |> 
    mutate_if(is.character, str_replace_all, pattern = "26.5", replacement = "27") |> 
    mutate_if(is.character, str_replace_all, pattern = "42.7", replacement = "43") |> 
    mutate_if(is.character, str_replace_all, pattern = "1.1511", replacement = "1") |> 
    mutate_if(is.character, str_replace_all, pattern = "0.58942", replacement = "1") |> 
    
    # convert to factor ----
  mutate(microplastics_lr = as_factor(microplastics_lr)) |> 
    
    # sum ----
  group_by(microplastics_lr) |>
    count() |>
    ungroup() |>
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

plot_q17b_microplastics_pre <- function(data){
  
  ggplot(data, aes(x = fct_reorder(microplastics_lr, desc(n)), y = n, label = perc_label, fill = microplastics_lr)) +
    geom_col() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Response",
         title = "How many pieces of microplastic do we predict will be present\nin a location with 45 days of rain per year (round your answer up to\nthe nearest integer)?",
         caption = "Question 17b (free response)") +
    scale_fill_manual(values = pal, limits = names(pal)) +
    meds_theme() +
    theme(
      legend.position = "none"
    )
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##----------------- QUESTION 18A (FAMILIARITY WITH PROB DIST)-------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q18a_familiar_prob_dist_pre <- function(PLO_data_clean){
  
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

plot_q18a_familiar_prob_dist_pre <- function(data){
  
  ggplot(data, aes(x = prob_dist, y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    coord_flip() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Familiarity level",
         title = "How familiar are you with the term probability distribution?",
         caption = "Question 18a (choosing '1 (never heard of it)' skips respondent to question 19)") +
    meds_theme()
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##---------------------- QUESTION 18B (PROB DIST TERMS)-------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# DOES NOT CORRECTLY CALCULATE; DO NOT USE!
clean_q18b_prob_dist_terms_pre <- function(PLO_data_clean){
  
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

plot_q18b_prob_dist_terms_pre <- function(data){
  
  ggplot(data, aes(x = fct_reorder(prob_dist_terms, desc(n)), y = n, label = perc_label, fill = prob_dist_terms)) +
    geom_col() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Selection",
         title = "Which of the following terms are used to describe probability\ndistributions (select all that apply)?",
         caption = "Question 18b") +
    scale_fill_manual(values = pal, limits = names(pal)) +
    meds_theme() +
    theme(
      legend.position = "none"
    )
  
}

clean_q18b_FULLY_CORRECT_pre <- function(PLO_data_clean){
  
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

plot_q18b_FULLY_CORRECT_pre <- function(data){
  
  ggplot(data, aes(x = fct_reorder(correctness, desc(total)), y = total, label = perc_label, fill = correctness)) +
    geom_col() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Did they get the question fully correct?",
         caption = "Question 18b") +
    scale_fill_manual(values = pal, limits = names(pal)) +
    meds_theme() +
    theme(
      legend.position = "none"
    )
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------- QUESTION 19A (FAMILIARITY WITH FXNS)----------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q19a_familiar_functions_pre <- function(PLO_data_clean){
  
  # to iterate over ----
  options <- c("1 (never heard of it)", "2", "3 (vague sense of what it means)", "4", "5 (very familiar)")
  
  df1 <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(term_function) |> 
    
    # sum ----
  group_by(term_function) |>
    count() |>
    ungroup() 
  
  # # ADDING BC NO ONE SELECTED THE FOLLOWING OPTIONS ----
  # add_row(term_function = "2", n = 0) |>
  #   add_row(term_function = "1 (never heard of it)", n = 0) |>
  
  for (i in 1:length(options)){
    
    cat_name <- options[i]
    
    # if category already exists in df, skip to next one
    if (cat_name %in% pull(df1[,1])) {
      
      message(cat_name, " already exists. Moving to next option.")
      df1 <- df1
      
      # if category doesn't already exist, add it with n = 0 so that it still shows up on plot
    } else {
      
      message(cat_name, " does not exist. Adding now.")
      new_row <- data.frame(term_function = cat_name, n = 0)
      df1 <- rbind(df1, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  # finish wrangling ----
  df2 <- df1 |> 
    
    # reorder factors ----
  mutate(term_function = fct_relevel(term_function, 
                                     c("1 (never heard of it)", "2", 
                                       "3 (vague sense of what it means)", "4", "5 (very familiar)"))) |>
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

plot_q19a_familiar_functions_pre <- function(data){
  
  ggplot(data, aes(x = term_function, y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    coord_flip() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Familiarity level",
         title = "How familiar are you with the term 'function' as it relates to\nprogramming?",
         caption = "Question 19a (choosing '1 (never heard of it)' skips respondent to question 20)") +
    meds_theme()
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##----------------------- QUESTION 19B (WRITING FXNS)---------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q19b_writing_functions_pre <- function(PLO_data_clean){
  
  # to iterate over ----
  options <- c("5 (very comfortable)", "4", "3", "2", "1 (not at all comfortable)")
  
  df1 <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(writing_functions) |> 
    
    # sum ----
  group_by(writing_functions) |>
    count() |>
    ungroup() 
  
  # ADDING BC NO ONE SELECTED THE FOLLOWING OPTIONS ----
  # add_row(writing_functions = "1 (not at all)", n = 0) |>
  
  for (i in 1:length(options)){
    
    cat_name <- options[i]
    
    # if category already exists in df, skip to next one
    if (cat_name %in% pull(df1[,1])) {
      
      message(cat_name, " already exists. Moving to next option.")
      df1 <- df1
      
      # if category doesn't already exist, add it with n = 0 so that it still shows up on plot
    } else {
      
      message(cat_name, " does not exist. Adding now.")
      new_row <- data.frame(writing_functions = cat_name, n = 0)
      df1 <- rbind(df1, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  # finish wrangling ----
  df2 <- df1 |> 
    
    # reorder factors ----
  mutate(writing_functions = fct_relevel(writing_functions, 
                                         c("1 (not at all comfortable)", "2", 
                                           "3", "4", "5 (very comfortable)"))) |>
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

plot_q19b_writing_functions_pre <- function(data){
  
  ggplot(data, aes(x = writing_functions, y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    coord_flip() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Comfort level",
         title = "How comfortable are you creating a function in code?",
         caption = "Question 19b (choosing '1 (not at all)' skips respondent to question 20)") +
    meds_theme()
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------------- QUESTION 19C (FXN OUTPUT)---------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q19c_fxn_output_pre <- function(PLO_data_clean){
  
  PLO_data_clean |> 
    
    # select necessary cols ----
  select(fxn_output) |> 
    
    # sum ----
  group_by(fxn_output) |>
    count() |>
    ungroup() |> 
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

plot_q19c_fxn_output_pre <- function(data){
  
  ggplot(data, aes(x = fct_reorder(fxn_output, desc(n)), y = n, label = perc_label, fill = fxn_output)) +
    geom_col() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Selection",
         title = "What is the value of power_turbine_A?",
         caption = "Question 19c") +
    scale_fill_manual(values = pal, limits = names(pal)) +
    meds_theme() +
    theme(
      legend.position = "none"
    )
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------ QUESTION 20A (RUN ENVIRONMENTAL MODEL)---------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q20a_run_env_mod_pre <- function(PLO_data_clean){
  
  # to iterate over ----
  options <- c("Yes", "No")
  
  df1 <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(run_environ_model) |> 
    
    # sum ----
  group_by(run_environ_model) |>
    count() |>
    ungroup()
  
  # ADDING BC NO ONE SELECTED THE FOLLOWING OPTIONS ----
  # add_row(run_environ_model = "No", n = 0) |>
  for (i in 1:length(options)){
    
    cat_name <- options[i]
    
    # if category already exists in df, skip to next one
    if (cat_name %in% pull(df1[,1])) {
      
      message(cat_name, " already exists. Moving to next option.")
      df1 <- df1
      
      # if category doesn't already exist, add it with n = 0 so that it still shows up on plot
    } else {
      
      message(cat_name, " does not exist. Adding now.")
      new_row <- data.frame(run_environ_model = cat_name, n = 0)
      df1 <- rbind(df1, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  # finish wrangling ----
  df2 <- df1 |> 
    
    # coerce to factors ----
  mutate(run_environ_model = as_factor(run_environ_model)) |> 
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

plot_q20a_run_env_mod_pre <- function(data){
  
  ggplot(data, aes(x = fct_reorder(run_environ_model, desc(n)), y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Selection",
         title = "Have you run a model to learn something about (or predict\nsomething about) the environment?",
         caption = "Question 20a (choosing 'No' skips respondent to question 21)") +
    meds_theme() 
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------- QUESTION 20B (SENSITIVITY ANALYSIS)-----------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q20b_sa_pre <- function(PLO_data_clean){
  
  # to iterate over ----
  options <- c("Yes", "No")
  
  df1 <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(sensitivity_analysis) |> 
    
    # sum ----
  group_by(sensitivity_analysis) |>
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
      new_row <- data.frame(sensitivity_analysis = cat_name, n = 0)
      df1 <- rbind(df1, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  # finish wrangling ----
  df2 <- df1 |> 
    
    # coerce to factors ----
  mutate(sensitivity_analysis = as_factor(sensitivity_analysis)) |> 
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

plot_q20b_sa_pre <- function(data){
  
  ggplot(data, aes(x = fct_reorder(sensitivity_analysis, desc(n)), y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Selection",
         title = "Have you done a sensitivity analysis to assess how model results\nchange with changes in inputs or parameters?",
         caption = "Question 20b (choosing 'No' skips respondent to question 21)") +
    meds_theme()  
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------- QUESTION 20C (PARAMETER INTERACTIONS)---------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q20c_param_int_pre <- function(PLO_data_clean){
  
  PLO_data_clean |> 
    
    # select necessary cols ----
  select(param_interactions) |> 
    
    # replace NA (from respondent who selected 'No' for previous question) ----
  replace_na(list(param_interactions = "No response")) |> 
    
    # sum ----
  group_by(param_interactions) |>
    count() |>
    ungroup() |> 
    
    # coerce to factors ----
  mutate(param_interactions = as_factor(param_interactions)) |> 
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

plot_q20c_param_int_pre <- function(data){
  
  ggplot(data, aes(x = fct_reorder(param_interactions, desc(n)), y = n, label = perc_label, fill = param_interactions)) +
    geom_col() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Selection",
         title = "If you want to explore how parameter interactions impact model\nresults, you would do...",
         caption = "Question 20c; No Response == answered 'No' to Q20b") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    scale_fill_manual(values = pal, limits = names(pal)) +
    meds_theme() +
    theme(
      legend.position = "none"
    )
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------- QUESTION 21A (COMFORT WORKING WITH SPATIAL DATA)----------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q21a_comfort_spatial_pre <- function(PLO_data_clean){
  
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

plot_q21a_comfort_spatial_pre <- function(data){
  
  ggplot(data, aes(x = spatial_data, y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    coord_flip() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Comfort level",
         title = "How comfortable are you working with spatial data?",
         caption = "Question 21a (choosing '1 (never worked with it before)' skips respondent to question 22)") +
    meds_theme()
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##----------------- QUESTION 21B (REPRESENTING SPATIAL DATA)--------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q21b_rep_spatial_pre <- function(PLO_data_clean){
  
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

plot_q21b_rep_spatial_pre <- function(data){
  
  ggplot(data, aes(x = fct_reorder(rep_spatial_data, desc(n)), y = n, label = perc_label, fill = rep_spatial_data)) +
    geom_col() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Selection",
         title = "What are the two primary ways of representing spatial data (select\ntwo)?",
         caption = "Question 21b") +
    scale_fill_manual(values = pal, limits = names(pal)) +
    meds_theme() +
    theme(
      legend.position = "none"
    )
  
}

clean_q21b_FULLY_CORRECT_pre <- function(PLO_data_clean){
  
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

plot_q21b_FULLY_CORRECT_pre <- function(data){
  
  ggplot(data, aes(x = fct_reorder(correctness, desc(n)), y = n, label = perc_label, fill = correctness)) +
    geom_col() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Did they get the question fully correct?",
         caption = "Question 21b") +
    scale_fill_manual(values = pal, limits = names(pal)) +
    meds_theme() +
    theme(
      legend.position = "none"
    )
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------- QUESTION 21C (VECTOR OR RASTER)-------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q21c_vec_ras_pre <- function(PLO_data_clean){
  
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

plot_q21c_vec_ras_pre <- function(data){
  
  ggplot(data, aes(x = fct_reorder(vec_or_ras, desc(n)), y = n, label = perc_label, fill = vec_or_ras)) +
    geom_col() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Selection",
         title = "Is the following a vector or raster?",
         caption = "Question 21c") +
    scale_fill_manual(values = pal, limits = names(pal)) +
    meds_theme() +
    theme(
      legend.position = "none"
    )
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------- QUESTION 22A (COMFORT WITH REMOTE SENSING DATA)-----------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q22a_comfort_rs_pre <- function(PLO_data_clean){
  
  # to iterate over ----
  options <- c("1 (never worked with it before)", "2", "3", "4", "5 (work with it all the time)")
  
  df1 <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(remote_sense_comfort) |> 
    
    # sum ----
  group_by(remote_sense_comfort) |>
    count() |>
    ungroup() 
  
  # ADDING BC NO ONE SELECTED THE FOLLOWING OPTIONS ----
  # add_row(remote_sense_comfort = "1 (never worked with it before)", n = 0) |>
  for (i in 1:length(options)){
    
    cat_name <- options[i]
    
    # if category already exists in df, skip to next one
    if (cat_name %in% pull(df1[,1])) {
      
      message(cat_name, " already exists. Moving to next option.")
      df1 <- df1
      
      # if category doesn't already exist, add it with n = 0 so that it still shows up on plot
    } else {
      
      message(cat_name, " does not exist. Adding now.")
      new_row <- data.frame(remote_sense_comfort = cat_name, n = 0)
      df1 <- rbind(df1, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  # finish wrangling ----
  df2 <- df1 |> 
    
    # reorder factors ----
  mutate(remote_sense_comfort = fct_relevel(remote_sense_comfort, 
                                            c("1 (never worked with it before)", "2", 
                                              "3", "4", "5 (work with it all the time)"))) |>
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

plot_q22a_comfort_rs_pre <- function(data){
  
  ggplot(data, aes(x = remote_sense_comfort, y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    coord_flip() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Comfort level",
         title = "How comfortable are you working with remotely sensed data?",
         caption = "Question 22a (choosing '1 (never worked with it before)' skips respondent to question 23)") +
    meds_theme()
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------- QUESTION 22B (REMOTE SENSING SUN)-----------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q22b_rs_sun_pre <- function(PLO_data_clean){
  
  PLO_data_clean |> 
    
    # select necessary cols ----
  select(remote_sensing_sun) |> 
    
    # sum ----
  group_by(remote_sensing_sun) |>
    count() |>
    ungroup() |> 
    
    # ADDING BC NO ONE SELECTED THE FOLLOWING OPTIONS ----
  add_row(remote_sensing_sun = "I'm not sure", n = 0) |>
    
    # coerce to factor ----
  mutate(remote_sensing_sun = as_factor(remote_sensing_sun)) |> 
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

plot_q22b_rs_sun_pre <- function(data){
  
  ggplot(data, aes(x = fct_reorder(remote_sensing_sun, desc(n)), y = n, label = perc_label, fill = remote_sensing_sun)) +
    geom_col() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Selection",
         title = "The type of remote sensing that relies on reflected radiation\ngenerated by the sun is called ___? (fill in the blank)",
         caption = "Question 22b") +
    scale_fill_manual(values = pal, limits = names(pal)) +
    meds_theme() +
    theme(
      legend.position = "none"
    )
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------- QUESTION 23A (COMFORT WITH MAP PROJECTIONS)-------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q23a_comfort_map_proj_pre <- function(PLO_data_clean){
  
  # to iterate over ----
  options <- c("1 (never worked with it before)", "2", "3", "4", "5 (work with it all the time)")
  
  df1 <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(map_proj_comfort) |> 
    
    # sum ----
  group_by(map_proj_comfort) |>
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
      new_row <- data.frame(map_proj_comfort = cat_name, n = 0)
      df1 <- rbind(df1, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  # finish wrangling ----
  df2 <- df1 |> 
    
    # coerce to factors ----
  mutate(map_proj_comfort = as_factor(map_proj_comfort)) |> 
    
    # reorder factors ----
  mutate(map_proj_comfort = fct_relevel(map_proj_comfort,
                                        c("1 (never worked with it before)", "2",
                                          "3", "4", "5 (work with it all the time)"))) |>
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

plot_q23a_comfort_map_proj_pre <- function(data){
  
  ggplot(data, aes(x = map_proj_comfort, y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    coord_flip() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Comfort level",
         title = "How comfortable are you working with map projections?",
         caption = "Question 23a (choosing '1 (never worked with it before)' skips respondent to question 24)") +
    meds_theme()
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##----------------------- QUESTION 23B (REPROJECTION)---------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q23b_reproj_pre <- function(PLO_data_clean){
  
  PLO_data_clean |>
    
    # select necessary cols ----
  select(convert_geo_to_coord) |>
    
    # sum ----
  group_by(convert_geo_to_coord) |>
    count() |>
    ungroup() |>
    
    # replace NA (from respondent who selected 'No' for previous question) ----
  replace_na(list(convert_geo_to_coord = "No response")) |> 
    
    # coerce to factors ----
  mutate(convert_geo_to_coord = as_factor(convert_geo_to_coord)) |>
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

plot_q23b_reproj_pre <- function(data){
  
  ggplot(data, aes(x = fct_reorder(convert_geo_to_coord, desc(n)), y = n, label = perc_label, fill = convert_geo_to_coord)) +
    geom_col() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Selection",
         title = "Converting from a geographic to projected coordinate system\n reprojects data from ___ (fill in the blank)",
         caption = "Question 23b") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    scale_fill_manual(values = pal, limits = names(pal)) +
    meds_theme() +
    theme(
      legend.position = "none"
    )
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##----------- QUESTION 24A (FAMILIARITY WITH REFLECTANCE SPECTRA)---------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q24a_familiarity_rs_pre <- function(PLO_data_clean){
  
  # to iterate over ----
  options <- c("1 (never heard of it)", "2", "3 (vague sense of what it means)", "4", "5 (very familiar)")
  
  df1 <- PLO_data_clean |>
    
    # select necessary cols ----
  select(reflec_spec) |>
    
    # sum ----
  group_by(reflec_spec) |>
    count() |>
    ungroup() 
  
  # ADDING BC NO ONE SELECTED THE FOLLOWING OPTIONS ----
  # add_row(reflec_spec = "5 (very familiar)", n = 0) |>
  for (i in 1:length(options)){
    
    cat_name <- options[i]
    
    # if category already exists in df, skip to next one
    if (cat_name %in% pull(df1[,1])) {
      
      message(cat_name, " already exists. Moving to next option.")
      df1 <- df1
      
      # if category doesn't already exist, add it with n = 0 so that it still shows up on plot
    } else {
      
      message(cat_name, " does not exist. Adding now.")
      new_row <- data.frame(reflec_spec = cat_name, n = 0)
      df1 <- rbind(df1, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  # finish wrangling ----
  df2 <- df1 |> 
    
    # reorder factors ----
  mutate(reflec_spec = fct_relevel(reflec_spec,
                                   c("1 (never heard of it)", "2",
                                     "3 (vague sense of what it means)", "4", "5 (very familiar)"))) |>
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

plot_q24a_familiarity_rs_pre <- function(data){
  
  ggplot(data, aes(x = reflec_spec, y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    coord_flip() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Familiarity level",
         title = "How familiar are you with the term reflectance spectra?",
         caption = "Question 24a (choosing '1 (never heard of it)' skips respondent to question 25)") +
    meds_theme()
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------- QUESTION 24B (VEGETATION WAVELENGTH)----------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q24b_veg_wave_pre <- function(PLO_data_clean){
  
  PLO_data_clean |> 
    
    # select necessary cols ----
  select(veg_wavelength) |> 
    
    # sum ----
  group_by(veg_wavelength) |>
    count() |>
    ungroup() |>
    
    # replace NA (from respondents who selected 'No' for previous question) ----
  replace_na(list(veg_wavelength = "No response")) |> 
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

plot_q24b_veg_wave_pre <- function(data){
  
  ggplot(data, aes(x = fct_reorder(veg_wavelength, desc(n)), y = n, label = perc_label, fill = veg_wavelength)) +
    geom_col() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Selection",
         title = "Of the following wavelengths, which one does vegetation reflect the\nmost?",
         caption = "Question 24b") +
    scale_fill_manual(values = pal, limits = names(pal)) +
    meds_theme() +
    theme(
      legend.position = "none"
    )
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##-------------------- QUESTION 25A (FAMILIARITY WITH ML)-----------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q25a_familiar_ml_pre <- function(PLO_data_clean){
  
  # to iterate over ----
  options <- c("1 (never heard of either of these terms)", "2",
               "3 (vague sense of these terms, but not why they are distinct from one another)", 
               "4", "5 (very familiar with both concepts and how they differ)")
  
  df1 <- PLO_data_clean |>
    
    # select necessary cols ----
  select(sup_vs_unsup_learn) |>
    
    # sum ----
  group_by(sup_vs_unsup_learn) |>
    count() |>
    ungroup() 
  
  # ADDING BC NO ONE SELECTED THE FOLLOWING OPTIONS ----
  # add_row(sup_vs_unsup_learn = "1 (never heard of either of these terms)", n = 0) |>
  for (i in 1:length(options)){
    
    cat_name <- options[i]
    
    # if category already exists in df, skip to next one
    if (cat_name %in% pull(df1[,1])) {
      
      message(cat_name, " already exists. Moving to next option.")
      df1 <- df1
      
      # if category doesn't already exist, add it with n = 0 so that it still shows up on plot
    } else {
      
      message(cat_name, " does not exist. Adding now.")
      new_row <- data.frame(sup_vs_unsup_learn = cat_name, n = 0)
      df1 <- rbind(df1, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  # finish wrangling ----
  df2 <- df1 |> 
    
    # reorder factors ----
  mutate(sup_vs_unsup_learn = fct_relevel(sup_vs_unsup_learn,
                                          c("1 (never heard of either of these terms)", "2",
                                            "3 (vague sense of these terms, but not why they are distinct from one another)", "4", "5 (very familiar with both concepts and how they differ)"))) |>
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

plot_q25a_familiar_ml_pre <- function(data){
  
  ggplot(data, aes(x = sup_vs_unsup_learn, y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    coord_flip() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Familiarity level",
         title = "How familiar are you with the difference between supervised and\nunsupervised learning?",
         caption = "Question 25a (choosing '1 (never heard of it)' skips respondent to question 26)") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    meds_theme()
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------- QUESTION 25B (UNUSPERVISED ALGO)------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q25b_unsup_alg_pre <- function(PLO_data_clean){
  
  PLO_data_clean |>
    
    # select necessary cols ----
  select(implemented_algo) |>
    
    # sum ----
  group_by(implemented_algo) |>
    count() |>
    ungroup() |>
    
    # ADDING BC NO ONE SELECTED THE FOLLOWING OPTIONS ----
  add_row(implemented_algo = "1 (definitely not)", n = 0) |>
    
    # reorder factors ----
  mutate(implemented_algo = fct_relevel(implemented_algo,
                                        c("1 (definitely not)", "2",
                                          "3 (maybe, but I'm not sure)", "4", "5 (yes)"))) |>
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

plot_q25b_unsup_alg_pre <- function(data){
  
  ggplot(data, aes(x = implemented_algo, y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    coord_flip() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Selection",
         title = "Have you ever implemented an unsupervised learning algorithm?",
         caption = "Question 25b (choosing '1 (definitely not)' skips respondent to question 26)") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    meds_theme()
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------------- QUESTION 25C (KMEANS)-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q25c_kmeans_pre <- function(PLO_data_clean){
  
  PLO_data_clean |>
    
    # select necessary cols ----
  select(kmeans) |>
    
    # sum ----
  group_by(kmeans) |>
    count() |>
    ungroup() |>
    
    # coerce to factor ----
  mutate(kmeans = as_factor(kmeans)) |> 
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

plot_q25c_kmeans_pre <- function(data){
  
  ggplot(data, aes(x = fct_reorder(kmeans, desc(n)), y = n, label = perc_label, fill = kmeans)) +
    geom_col() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Selection",
         title = "K-means clustering is an example of a(n) ___ learning approach\nbecause it ___ (fill in the blanks).",
         caption = "Question 25c") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    scale_fill_manual(values = pal, limits = names(pal)) +
    meds_theme() +
    theme(
      legend.position = "none"
    )
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------------ QUESTION 26A (DIVIDE DATA)---------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q26a_div_data_pre <- function(PLO_data_clean){
  
  # to iterate over ----
  options <- c("1 (never heard of it)", "2",
               "3 (vague sense of what it means)", 
               "4", "5 (very familiar)")
  
  df1 <- PLO_data_clean |>
    
    # select necessary cols ----
  select(ml_div_data) |>
    
    # sum ----
  group_by(ml_div_data) |>
    count() |>
    ungroup() 
  
  # ADDING BC NO ONE SELECTED THE FOLLOWING OPTIONS ----
  # add_row(ml_div_data = "1 (never heard of it)", n = 0) |>
  #   add_row(ml_div_data = "2", n = 0) |>
  for (i in 1:length(options)){
    
    cat_name <- options[i]
    
    # if category already exists in df, skip to next one
    if (cat_name %in% pull(df1[,1])) {
      
      message(cat_name, " already exists. Moving to next option.")
      df1 <- df1
      
      # if category doesn't already exist, add it with n = 0 so that it still shows up on plot
    } else {
      
      message(cat_name, " does not exist. Adding now.")
      new_row <- data.frame(ml_div_data = cat_name, n = 0)
      df1 <- rbind(df1, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  # finish wrangling ----
  df2 <- df1 |> 
    
    # reorder factors ----
  mutate(ml_div_data = fct_relevel(ml_div_data,
                                   c("1 (never heard of it)", "2",
                                     "3 (vague sense of what it means)", 
                                     "4", "5 (very familiar)"))) |>
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

plot_q26a_div_data_pre <- function(data){
  
  ggplot(data, aes(x = ml_div_data, y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    coord_flip() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Familiarity level",
         title = "How familiar are you with the procedure that divides your data into\nseparate “training”, “validation”, and “testing” sets?",
         caption = "Question 26a (choosing '1 (never heard of it)' skips respondent to question 27)") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    meds_theme()
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------- QUESTION 26B (TRAIN VALIDATE SPLIT)-----------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q26b_tvs_pre <- function(PLO_data_clean){
  
  # to iterate over ----
  options <- c("1 (never)", "2", "3", "4", "5 (all the time)")
  
  df1 <- PLO_data_clean |>
    
    # select necessary cols ----
  select(train_valid_split) |>
    
    # sum ----
  group_by(train_valid_split) |>
    count() |>
    ungroup()
  
  # ADDING BC NO ONE SELECTED THE FOLLOWING OPTIONS ----
  # add_row(train_valid_split = "1 (never)", n = 0) |>
  for (i in 1:length(options)){
    
    cat_name <- options[i]
    
    # if category already exists in df, skip to next one
    if (cat_name %in% pull(df1[,1])) {
      
      message(cat_name, " already exists. Moving to next option.")
      df1 <- df1
      
      # if category doesn't already exist, add it with n = 0 so that it still shows up on plot
    } else {
      
      message(cat_name, " does not exist. Adding now.")
      new_row <- data.frame(train_valid_split = cat_name, n = 0)
      df1 <- rbind(df1, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  # finish wrangling ----
  df2 <- df1 |> 
    
    # reorder factors ----
  mutate(train_valid_split = fct_relevel(train_valid_split,
                                         c("1 (never)", "2", "3", "4", "5 (all the time)"))) |>
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

plot_q26b_tvs_pre <- function(data){
  
  ggplot(data, aes(x = train_valid_split, y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    coord_flip() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Selection",
         title = "How often have you implemented a train, validation, test split?",
         caption = "Question 26b (choosing '1 (never)' skips respondent to question 27)") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    meds_theme()
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------- QUESTION 26C (MODEL PERFORMANCE)------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q26c_mod_perf_pre <- function(PLO_data_clean){
  
  PLO_data_clean |>
    
    # select necessary cols ----
  select(learning_from_model) |>
    
    # split strings by `,` delim ----
  separate_longer_delim(learning_from_model, delim = ",") |>
    
    # sum ----
  group_by(learning_from_model) |>
    count() |>
    ungroup() |>
    
    # ADDING BC NO ONE SELECTED THE FOLLOWING OPTIONS ----
  add_row(learning_from_model = "My model is likely to perform very well when applied to new data", n = 0) |>
    add_row(learning_from_model = "My test set has data entry errors in it", n = 0) |>
    
    # add col for percentages ----
  mutate(percentage = round((n/q26c_num_answers)*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

plot_q26c_mod_perf_pre <- function(data){
  
  ggplot(data, aes(x = fct_reorder(learning_from_model, desc(n)), y = n, label = perc_label, fill = learning_from_model)) +
    geom_col() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Selection",
         title = "If my model performs very well in my training set, moderately well\nin my validation set, and poorly in my test set, what do I learn from\nthis (choose all that apply)?",
         caption = "Question 26c") +
    scale_fill_manual(values = pal, limits = names(pal)) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    meds_theme() +
    theme(
      legend.position = "none"
    )
  
}

clean_q26c_FULLY_CORRECT_pre <- function(PLO_data_clean){
  
  PLO_data_clean |>
    
    # select necessary cols ----
  select(learning_from_model) |>
    
    # sum ----
  group_by(learning_from_model) |>
    count() |>
    ungroup() |>
    
    # add correct or incorrect label ----
  mutate(correctness = case_when(
    learning_from_model == "My model is overfitting the training set" ~ "no",
    learning_from_model == "My model is unlikely to perform well when applied to new data,My model is overfitting the training set" ~ "yes",
  )) |>
    
    # coerce data types ----
  mutate(correctness = as_factor(correctness)) |>
    
    # sum ----
  group_by(correctness) |>
    summarize(total = sum(n)) |>
    ungroup() |>
    
    # add col for percentages ----
  mutate(percentage = round((total/(sum(total)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

plot_q26c_FULLY_CORRECT_pre <- function(data){
  
  ggplot(data, aes(x = fct_reorder(correctness, desc(total)), y = total, label = perc_label, fill = correctness)) +
    geom_col() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Did they get the question fully correct?",
         caption = "Question 26c") +
    scale_fill_manual(values = pal, limits = names(pal)) +
    meds_theme() +
    theme(
      legend.position = "none"
    )
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------------ QUESTION 27 (DATA JUSTICE)---------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q27_data_justice_pre <- function(PLO_data_clean){
  
  # to iterate over ----
  options <- c("1 (never heard of it)", "2", 
               "3 (vague sense of what it means)", "4", "5 (very familiar)")
  
  df1 <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(data_justice) |> 
    
    # sum ----
  group_by(data_justice) |>
    count() |>
    ungroup() 
  
  # ADDING BC NO ONE SELECTED THE FOLLOWING OPTIONS ----
  # add_row(data_justice = "2", n = 0) |>
  for (i in 1:length(options)){
    
    cat_name <- options[i]
    
    # if category already exists in df, skip to next one
    if (cat_name %in% pull(df1[,1])) {
      
      message(cat_name, " already exists. Moving to next option.")
      df1 <- df1
      
      # if category doesn't already exist, add it with n = 0 so that it still shows up on plot
    } else {
      
      message(cat_name, " does not exist. Adding now.")
      new_row <- data.frame(data_justice = cat_name, n = 0)
      df1 <- rbind(df1, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  # finish wrangling ----
  df2 <- df1 |> 
    
    # reorder factors ----
  mutate(data_justice = fct_relevel(data_justice, 
                                    c("1 (never heard of it)", "2", 
                                      "3 (vague sense of what it means)", "4", "5 (very familiar)"))) |>
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))  
  
}

plot_q27_data_justice_pre <- function(data){
  
  ggplot(data, aes(x = data_justice, y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    coord_flip() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Familiarity level",
         title = "How familiar are you with the term Data Justice?",
         caption = "Question 27") +
    meds_theme()
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##---------------------------- QUESTION 28 (BIAS)-------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q28_bias_pre <- function(PLO_data_clean){
  
  # to iterate over ----
  options <- c("1 (strongly disagree)", "2", 
               "3 (neutral)", "4", "5 (strongly agree)")
  
  df1 <- PLO_data_clean |>
    
    # select necessary cols ----
  select(bias) |>
    
    # sum ----
  group_by(bias) |>
    count() |>
    ungroup() 
  
  # ADDING BC NO ONE SELECTED THE FOLLOWING OPTIONS ----
  # add_row(bias = "2", n = 0) |>
  #   add_row(bias = "1 (strongly disagree)", n = 0) |> 
  for (i in 1:length(options)){
    
    cat_name <- options[i]
    
    # if category already exists in df, skip to next one
    if (cat_name %in% pull(df1[,1])) {
      
      message(cat_name, " already exists. Moving to next option.")
      df1 <- df1
      
      # if category doesn't already exist, add it with n = 0 so that it still shows up on plot
    } else {
      
      message(cat_name, " does not exist. Adding now.")
      new_row <- data.frame(bias = cat_name, n = 0)
      df1 <- rbind(df1, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  # finish wrangling ----
  df2 <- df1 |> 
    
    # reorder factors ----
  mutate(bias = fct_relevel(bias, 
                            c("1 (strongly disagree)", "2", 
                              "3 (neutral)", "4", "5 (strongly agree)"))) |>
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

plot_q28_bias_pre <- function(data){
  
  ggplot(data, aes(x = bias, y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    coord_flip() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Agreement level",
         title = "I can identify areas of bias in approaches to data analysis.",
         caption = "Question 28") +
    meds_theme()
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##----------------------- QUESTION 29 (CREATE DATA VIZ)-------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q29_create_viz_pre <- function(PLO_data_clean){
  
  # to iterate over ----
  options <- c("1 (never done it)", "2", 
               "3", "4", "5 (very comfortable, do it often)")
  
  df1 <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(data_viz_programming) |> 
    
    # sum ----
  group_by(data_viz_programming) |>
    count() |>
    ungroup()
  
  # ADDING BC NO ONE SELECTED THE FOLLOWING OPTIONS ----
  # add_row(data_viz_programming = "1 (never done it)", n = 0) |>
  for (i in 1:length(options)){
    
    cat_name <- options[i]
    
    # if category already exists in df, skip to next one
    if (cat_name %in% pull(df1[,1])) {
      
      message(cat_name, " already exists. Moving to next option.")
      df1 <- df1
      
      # if category doesn't already exist, add it with n = 0 so that it still shows up on plot
    } else {
      
      message(cat_name, " does not exist. Adding now.")
      new_row <- data.frame(data_viz_programming = cat_name, n = 0)
      df1 <- rbind(df1, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  # finish wrangling ----
  df2 <- df1 |> 
    
    # reorder factors ----
  mutate(data_viz_programming = fct_relevel(data_viz_programming, 
                                            c("1 (never done it)", "2", 
                                              "3", "4", "5 (very comfortable, do it often)"))) |>
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

plot_q29_create_viz_pre <- function(data){
  
  ggplot(data, aes(x = data_viz_programming, y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    coord_flip() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Comfort level",
         title = "How comfortable are you with creating a data visualization using a\nprogramming language?",
         caption = "Question 29") +
    meds_theme()
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##---------------------- QUESTION 30 (IMPROVE DATA VIZ)-------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q30_improve_dv_pre <- function(PLO_data_clean){
  
  PLO_data_clean |>
    
    # select necessary cols ----
  select(improve_data_viz) |> 
    
    # split strings by `,` delim ----
  separate_longer_delim(improve_data_viz, " ")  |> 
    separate_longer_delim(improve_data_viz, ",") |> 
    separate_longer_delim(improve_data_viz, ".") |>  
    separate_longer_delim(improve_data_viz, ")") |>  
    separate_longer_delim(improve_data_viz, "(") |> 
    separate_longer_delim(improve_data_viz, ";") |> 
    filter(improve_data_viz != "") |> 
    filter(!improve_data_viz %in% c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "-")) |> 
    mutate(improve_data_viz = str_to_lower(improve_data_viz)) |> 
    group_by(improve_data_viz) |> 
    count() |> 
    arrange(desc(n)) |> 
    rename(word = improve_data_viz) |> 
    anti_join(y = stop_words)
  
}

plot_q30_improve_dv_pre <- function(data){
  
  wordcloud(data$word, freq = data$n,
            min.freq = 1, max.words = 85, 
            random.order = FALSE, rot.per = 0.35,
            scale = c(4, .5), colors = brewer.pal(8, "Dark2")) 
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##----------------------- QUESTION 31 (WHAT LANGUAGE)---------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_q31_lang_pre <- function(PLO_data_clean){
  
  # to iterate over ----
  options <- c("I'm not sure", "Python", "R", "SQL")
  
  # select var of interest ----
  df1 <- PLO_data_clean |> 
    
    # select necessary cols ----
  select(what_lang_is_this) |> 
    
    # sum ----
  group_by(what_lang_is_this) |>
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
      new_row <- data.frame(what_lang_is_this = cat_name, n = 0)
      df1 <- rbind(df1, new_row)
      
    }
    
    message("----------------------")
    
  } 
  
  # finish wrangling ----
  df2 <- df1 |> 
    
    # reorder factors ----
  mutate(what_lang_is_this = fct_relevel(what_lang_is_this,
                                         c("Python", "R", "SQL", "I'm not sure"))) |>
    
    # add col for percentages ----
  mutate(percentage = round((n/(sum(n)))*100, 1),
         perc_label = paste0(percentage, "%"))
  
}

plot_q31_lang_pre <- function(data){
  
  ggplot(data, aes(x = what_lang_is_this, y = n, label = perc_label, fill = what_lang_is_this)) +
    geom_col() +
    geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Language",
         title = "What programming language is the above code written in?",
         caption = "Question 31") +
    scale_fill_manual(values = pal, limits = names(pal)) +
    meds_theme() +
    theme(
      legend.position = "none"
    )
  
}
