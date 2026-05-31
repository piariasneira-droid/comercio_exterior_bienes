custom_theme_plotly <- function() {
  list(
    plot_bgcolor = 'rgba(255, 255, 255, 1)',
    paper_bgcolor = 'rgba(255, 255, 255, 1)',
    font = list(size = 14, family = "Calibri", color = "black"),
    xaxis = list(
      title = list(font = list(size = 14, family = "Calibri", color = "black")),
      tickfont = list(size = 14, family = "Calibri", color = "black"),
      ticks = "outside",
      tickcolor = "black",
      linecolor = "black",
      gridcolor = "lightgrey",
      gridwidth = 1,
      griddash = "dot",
      zeroline = FALSE
    ),
    yaxis = list(
      title = list(font = list(size = 14, family = "Calibri", color = "black")),
      tickfont = list(size = 14, family = "Calibri", color = "black"),
      ticks = "outside",
      tickcolor = "black",
      linecolor = "black",
      gridcolor = "lightgrey",
      gridwidth = 1,
      griddash = "dot",
      zeroline = FALSE
    ),
    legend = list(
      bgcolor = 'rgba(0, 0, 0, 0)',
      font = list(size = 12, family = "Calibri"),
      orientation = "h",
      x = 1,
      xanchor = "right",
      y = 1.02,
      yanchor = "bottom"
    ),
    margin = list(t = 40, b = 40, l = 40, r = 40)
  )
}