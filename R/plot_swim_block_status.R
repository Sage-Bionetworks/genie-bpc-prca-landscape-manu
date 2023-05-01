
# cast_block_dat is the output from make_cast_status_block()
# event_dat is the output from make_event_df()
# pt_subset is a vector of participants to plot.
plot_swim_block_status <- function(
  block_dat,
  block_var = "md_cast_status_f",
  event_dat,
  event_var = "event",
  pt_subset,
  line_pal = c("gray80", "#6699cc", "#ee99aa")
){
  
  if (!(block_var %in% names(block_dat))) {
    cli::cli_abort("block_var not in block_dat names().")
  }
  
  block_dat_sub <- block_dat %>% filter(record_id %in% pt_subset)
  event_dat_sub <- event_dat %>% filter(record_id %in% pt_subset)
  
  block_dat_sub %<>%
    arrange(tt_os_dx_yrs) %>%
    mutate(record_id = forcats::fct_inorder(record_id))
  event_dat_sub %<>% 
    mutate(record_id = factor(
      record_id, 
      levels = levels(block_dat_sub$record_id))
    )
  
  gg <- ggplot(block_dat_sub,
               aes(y = record_id, yend = record_id)) + 
    geom_segment(aes(x = dx_block_start, 
                     xend = dx_block_end, 
                     color = .data[[block_var]]),
                 linewidth = 1) +
    geom_point(data = event_dat_sub,
               aes(x = t_yrs, shape = .data[[event_var]]),
               stroke = 1) + 
    scale_x_continuous(
      name = "Time from diagnosis (years)",
      expand = expansion(add = c(0,0), mult = c(0, 0.05))) + 
    theme_bw() + 
    theme(
      legend.position = "top",
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    ) + 
    scale_color_manual(
      name = NULL, 
      values = line_pal,
      guide = guide_legend(ncol = 1)
    ) + 
    scale_shape_manual(
      name = NULL,
      values = c(18,21,4),
      guide = guide_legend(ncol = 1)
    )
  
  return(gg)
  
  
}

