#### Tema y paleta ----
colde1 <- "#2d5532"
colde2 <- "#6f6f4e"
colde3 <- "#b4d7b4"
colde4 <- "#ddd9c3"
colde5 <- "#a6a6a6"
colde6 <- "#d9d9d9"
colde7 <- "#a1a17a"
colde8 <- "#2d3535"
colde9 <- "#b4c7d7"

custom_theme <- ggplot2::theme(
  # Fondo y bordes
  panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
  plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
  
  # Títulos y texto
  plot.title = ggplot2::element_text(size = 7, face = "bold", color = "black", hjust = 0.5),
  plot.subtitle = ggplot2::element_text(size = 6, color = "black", hjust = 0.5),
  plot.title.position = "plot",
  axis.title = ggplot2::element_text(size = 7, color = "black", face = "bold"),
  axis.text = ggplot2::element_text(size = 6, color = "black"),
  axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, margin = ggplot2::margin(t = 0)),
  axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 0)),
  
  # Ejes y líneas de la cuadrícula
  axis.ticks = ggplot2::element_line(color = "black"),
  axis.line = ggplot2::element_line(color = "black"),
  panel.grid.major.x = ggplot2::element_blank(),
  panel.grid.minor.x = ggplot2::element_blank(),
  panel.grid.major.y = ggplot2::element_line(color = "black", size = 0.25, linetype = "dashed"),
  panel.grid.minor.y = ggplot2::element_line(color = "grey", size = 0.25, linetype = "dashed"),
  
  # Leyenda
  legend.box.background = ggplot2::element_rect(fill = "transparent", color = NA),
  legend.background = ggplot2::element_rect(fill = "transparent", color = NA),
  legend.position = "bottom",
  legend.justification = "right",
  legend.text = ggplot2::element_text(size = 6, family = "Calibri"),
  legend.title = ggplot2::element_text(size = 7, family = "Calibri", face = "bold"),
  legend.margin = ggplot2::margin(0, 0, 0, 0),
  legend.spacing = ggplot2::unit(0.3, "cm"),
  legend.key.size = ggplot2::unit(0.3, "cm"),
  legend.box.spacing = ggplot2::unit(0.1, "cm"),
  
  # Otros elementos
  plot.caption.position = "plot",
  plot.tag = ggplot2::element_text(size = 6, hjust = 0),
  plot.caption = ggplot2::element_text(size = 6, hjust = 0),
  strip.background = ggplot2::element_rect(fill = NA)
)