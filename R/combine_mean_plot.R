combine_mean_plot <- function(
  plot_list,
  shared_y_label = "Y label",
  rel_legend_size = 0.12,   # Reduce for smaller legend column!
  y_label_fontsize = 14,
  y_label_fontface = "bold"
) {
  # Extract legend from the first plot (with legend)
  legend <- cowplot::get_legend(plot_list[[1]])

  # Remove legends from ALL plots
  plot_list_nolegend <- lapply(plot_list, function(p) p + ggplot2::theme(legend.position = "none"))

  # Combine plots in a row
  combined_plots <- cowplot::plot_grid(plotlist = plot_list_nolegend, ncol = length(plot_list), align = "hv")

  # Add shared y label
  plots_with_ylabel <- cowplot::ggdraw() +
    cowplot::draw_label(
      shared_y_label, angle = 90, x = 0.02, y = 0.5, vjust = 0.5,
      fontface = y_label_fontface, size = y_label_fontsize
    ) +
    cowplot::draw_plot(combined_plots, x = 0.07, y = 0, width = 0.93, height = 1)

  # Arrange main plots and legend (legend on right, smaller width)
  final_plot <- cowplot::plot_grid(
    plots_with_ylabel,
    legend,
    ncol = 2,
    rel_widths = c(1, rel_legend_size)
  )
  return(final_plot)
}
