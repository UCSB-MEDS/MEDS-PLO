meds_theme <- theme_minimal() + 
  theme(
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0), family = "sanchez", size = 13),
    axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), family = "sanchez", size = 13), 
    axis.text = element_text(family = "nunito", size = 10),
    plot.title = element_text(margin = margin(t = 5, r = 0, b = 15, l = 0), size = 15, family = "sanchez"),
    plot.subtitle = element_text(margin = margin(t = 5, r = 0, b = 15, l = 0), size = 13, family = "sanchez"),
    plot.caption = element_text(family = "sanchez"),
    plot.title.position = "plot",
    legend.position = "none"
  )
