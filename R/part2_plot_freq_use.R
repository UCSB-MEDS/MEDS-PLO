plot_frequency_use <- function(data, title, caption){
  
  ggplot(data, aes(x = xvar, y = n, label = perc_label)) +
    geom_col(fill = "#047C91") +
    coord_flip() +
    geom_text(position = position_stack(vjust = 0.5), size = 4, color = "white", family = "nunito") +
    labs(y = "Number of MEDS students", x = "Frequency of use",
         title = title,
         caption = caption) +
    meds_theme
  
}