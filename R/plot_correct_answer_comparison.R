
plot_correct_answer_comparison <- function(data, title, subtitle, caption){
  
  data <- tidyr::complete(data, timepoint = c("Pre-MEDS", "Post-MEDS"), fill = list(percentage = 0, perc_label_long = "")) |> 
    mutate(timepoint = fct_relevel(timepoint, c("Pre-MEDS", "Post-MEDS")))
  
  ggplot(data, aes(x = timepoint, y = percentage)) +
    geom_col(aes(fill = timepoint)) +
    geom_text(aes(label = perc_label_long), 
              position = position_stack(vjust = 0.5), 
              size = 3, color = "white", family = "nunito") +
    labs(y = "% of respondents who\nanswered correctly",
         title = title,
         subtitle = subtitle,
         caption = caption) +
    scale_fill_manual(values = meds_pal) +
    scale_y_continuous(labels = scales::label_percent(scale = 1),
                       limits = c(0, 100)) +
    meds_theme() +
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      plot.subtitle = element_text(face = "bold")
    )
  
}
