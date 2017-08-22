# set ggplot default theme
theme_set(
  theme_bw() +
    theme(panel.border=element_rect(color=NA),
          plot.title = element_text(face='bold'),
          axis.title.x = element_text(hjust=1, face='italic', size=8))
)

