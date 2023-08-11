source("R/clean_PLO_data.R")
source("R/q3_gui.R")
library(googlesheets4)
library(tidyverse)
library(janitor)
meds2024_before <- read_sheet("https://docs.google.com/spreadsheets/d/1cJddylTu-eyb2fjbnVuvv-tvy6Knq1HfHjz_5zlW0fc/edit?usp=sharing")
meds2024_before_clean <- clean_PLO_data(meds2024_before)
medsJune2023 <- read_sheet("https://docs.google.com/spreadsheets/d/1Sq1rOmBP-g6iOCBS7NoexCj4K-j3OpeErMJ2Hxd3AHo/edit?usp=sharing")
medsJune2023_clean <- clean_PLO_data(medsJune2023)


test_fun <- function(PLO_data_clean){
  
  # to iterate over ----
  options <- c("Never", "Less than once per year", "Several times per year", "Monthly", "Weekly", "Daily")
  
  # select var of interest ----
  df1 <- medsJune2023_clean |> # PLO_data_clean
    
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
      df2 <- df1
      
      
      # if category doesn't already exist, add it with n = 0 so that it still shows up on plot
    } else {
      
      message(cat_name, " does not exist. Adding now.")
      df2 <- df1 |> add_row(point_and_click_gui = cat_name, n = 0) 
      
    }
    
    message("----------------------")
    # message("round ", i, " complete")
    
  } 
  
  # finish wrangling ----
  df3 <- df2 |> 

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
  return(df3)
  
}


# test it out
doesnt_have <- test_fun(medsJune2023_clean)
has <- test_fun(meds2024_before_clean)

