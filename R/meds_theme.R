meds_theme <- function() {
  theme_minimal() + 
    theme(
      axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0), family = "sanchez", size = 13),
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), family = "sanchez", size = 13), 
      axis.text = element_text(family = "nunito", size = 10),
      plot.title = element_text(margin = margin(t = 5, r = 0, b = 15, l = 0), size = 15, family = "sanchez"),
      plot.subtitle = element_text(margin = margin(t = 5, r = 0, b = 15, l = 0), size = 13, family = "sanchez"),
      plot.caption = element_text(family = "sanchez",
                                  margin = margin(t = 10, r = 0, b = 0, l = 0)),
      plot.title.position = "plot",
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(family = "nunito"),
      plot.margin = margin(t = 10, r = 25, b = 10, l = 10)
    )
}
  
