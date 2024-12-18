
#R figure settings
knitr::opts_chunk$set(
  fig.width = 9,
  fig.asp = .6,
  out.width = "90%"
)
theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d

knitr::opts_chunk$set(
  echo = FALSE,         #code is not displayed in output
  warning = FALSE,     #warnings false
  message = FALSE      #messages false
)
