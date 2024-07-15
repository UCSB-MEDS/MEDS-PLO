#' Wrangle data for for questions which asks respondents to  select how frequently they use a tool / workflow or rank their level of agreement with a particular statement
#'
#' @param all_PLO_data both Pre- and Post-MEDS data frame (named in class2024.qmd as `both_timepoints_clean`)
#' @param col_name column in `all_PLO_data` that corresponds to the target question responses (for any Qs 3-7) as a character string
#' @param categories a vector of frequency or rank options that a respondent is able to choose from for a particular question
#'
#' @return
#' @export
#'
#' @examples
clean_freq_rank_data <- function(all_PLO_data, col_name, categories){

  #........................to iterate over.........................
  options <- categories

  #..........count total responses by timepoint & option...........
  df <- all_PLO_data |>
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

  #.................calculate pre-MEDS percentages.................
  pre_meds <- pre_meds |>
    mutate(total_respondents = sum(n),
           percentage = round((n/total_respondents)*100, 1),
           perc_label = paste0(percentage, "%")) |>
    mutate(xvar = !!sym(col_name))
  
  #................calculate post-MEDS percentages.................
  post_meds <- post_meds |>
    mutate(total_respondents = sum(n),
           percentage = round((n/total_respondents)*100, 1),
           perc_label = paste0(percentage, "%")) |>
    mutate(xvar = !!sym(col_name))

  #................recombine pre- and post-MEDS data...............
  all_data <- rbind(pre_meds, post_meds) |>
    mutate(xvar = fct_relevel(xvar, options))

  return(all_data)

}




