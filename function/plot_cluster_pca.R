library(ggplot2)
library(glue)

plot_cluster_pca <- function(metrics_AF_std, arw_df, pcs = c("PC1", "PC2"), 
                             gr_col = NULL, pc_importance = NULL, t_adjust_pos = 1, 
                             lims = NULL) {
  stopifnot(length(pcs) == 2)
  
  # Extract axis names
  x_pc <- pcs[1]
  y_pc <- pcs[2]
  
  # Labels for axes
  x_lab <- if (!is.null(pc_importance) && x_pc %in% names(pc_importance)) {
    glue("{x_pc} ({pc_importance[x_pc]}%)")
  } else {
    x_pc
  }
  
  y_lab <- if (!is.null(pc_importance) && y_pc %in% names(pc_importance)) {
    glue("{y_pc} ({pc_importance[y_pc]}%)")
  } else {
    y_pc
  }
  
  # Plot
  p <- ggplot() +
    geom_point(
      data = metrics_AF_std,
      aes(x = .data[[x_pc]], y = .data[[y_pc]], color = gr),
      pch = 16, size = 1, show.legend = FALSE
    ) +
    geom_label(
      data = arw_df,
      aes(x = xend * t_adjust_pos, y = yend * t_adjust_pos, label = label),
      alpha = 0.7, size = 2.5, fontface = "bold"
    ) +
    geom_segment(
      data = arw_df,
      aes(x = x, y = y, xend = xend, yend = yend),
      arrow = arrow(length = unit(0.075, "inches")),
      linewidth = 0.5
    ) +
    scale_color_manual(values = gr_col) +
    labs(x = x_lab, y = y_lab) +
    coord_equal() +
    theme_bw()
  
  
  # Apply limits if provided
  if (!is.null(lims)) {
      p <- p + 
        scale_x_continuous(limits = lims) +
        scale_y_continuous(limits = lims)
      }
    
  p
}
