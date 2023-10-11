
plot_alt_by_inst <- function(
    dat,
    prop_var = "prop_alt",
    plot_title = "Alteration frequencies by institution",
    plot_subtitle = "Any alteration - oncogenic or not.",
    reorder = T
) {
  
  dat %<>% mutate(
    shape_scale_help = factor(
      institution %in% "All",
      levels = c(F, T)
    )
  )
  
  if (reorder) {
    # only works because "all" is first:
    dat %<>%
      arrange(institution, desc(.data[[prop_var]])) %>%
      mutate(hugo = fct_inorder(hugo))
  }
  
  gg <- ggplot(
    data = dat,
    aes(
      x = .data[[prop_var]],
      y = hugo,
      color = institution,
      shape = shape_scale_help,
      size = shape_scale_help
    )
  ) + 
    geom_hline(yintercept = 0:50, color = "gray90") + 
    geom_point() + 
    coord_cartesian(
      xlim = c(-0.01,NA)
    ) +
    scale_x_continuous(
      breaks = seq(0, 50, by = 5)/100,
      labels = paste(seq(0, 50, by = 5), "%"),
      name = "Proportion altered",
      expand = c(0,0)
    ) + 
    scale_shape_manual(
      values = c(19, 15),
      guide = 'none'
    ) + 
    scale_color_manual(
      values = c("black", '#ee7733', '#0077bb', '#33bbee', '#ee3377')
    ) + 
    scale_size_manual(values = c(1,2), guide = "none") + 
    scale_y_discrete(
      limits = rev,
      name = NULL
    ) + 
    labs(
      title = plot_title,
      subtitle = plot_subtitle
    ) + 
    theme_classic() + 
    theme(
      legend.position = 'bottom',
      plot.title.position = "plot"
    )
  
  return(gg)
  
}
