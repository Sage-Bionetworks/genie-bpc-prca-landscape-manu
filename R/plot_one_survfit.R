
plot_one_survfit <- function(
    dat,
    surv_form,
    pal = c('#bb5566', '#004488', '#ddaa33'),
    plot_title = NULL,
    plot_subtitle = NULL,
    x_title = "Years",
    risktable_prop = NULL
) {
  gg <- survfit2(surv_form, data = dat) %>%
    ggsurvfit() +
    add_risktable(
      risktable_stats = c(
        "n.risk",
        "cum.censor",
        "cum.event"
      ),
      hjust = 0,
      risktable_height = risktable_prop,
      size = 3.5  # default
    ) +
    scale_y_continuous(
      expand = c(0,0),
      label = scales::label_percent(),
      name = "Survival"
    ) +
    scale_x_continuous(
      name = x_title,
      expand = c(0,0),
      breaks = seq(0, 100, by = 2.5)
    ) +
    scale_color_manual(
      values = pal
    ) + 
    coord_cartesian(
      xlim = c(0, NA),
      ylim = c(0,1.01)
    ) +
    labs(
      title = plot_title,
      subtitle = plot_subtitle
    ) +
    theme(
      axis.title.y = element_blank(),
      plot.title.position = "plot",
      # prevents the axis tick label clipping:
      plot.margin=unit(c(.2,.2,.2,.2),"cm")
    )
  
  return(gg)
}
