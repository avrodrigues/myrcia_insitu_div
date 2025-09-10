library(plotly)
library(dplyr)
library(glue)

plot_cluster_pca_3d <- function(metrics_AF_std, arw_df, pcs = c("PC1", "PC2", "PC3"),
                                gr_col = NULL, pc_importance = NULL, t_adjust_pos = 1) {
  stopifnot(length(pcs) == 3)
  
  # Extract PC names
  x_pc <- pcs[1]
  y_pc <- pcs[2]
  z_pc <- pcs[3]
  
  # Axis labels (with variance explained if provided)
  x_lab <- if (!is.null(pc_importance) && x_pc %in% names(pc_importance)) {
    glue("{x_pc} ({pc_importance[[x_pc]]}%)")
  } else x_pc
  
  y_lab <- if (!is.null(pc_importance) && y_pc %in% names(pc_importance)) {
    glue("{y_pc} ({pc_importance[[y_pc]]}%)")
  } else y_pc
  
  z_lab <- if (!is.null(pc_importance) && z_pc %in% names(pc_importance)) {
    glue("{z_pc} ({pc_importance[[z_pc]]}%)")
  } else z_pc
  
  # Expand arrow dataframe so each arrow has two rows (start, end)
  arrows_long <- arw_df %>%
    rowwise() %>%
    do({
      tibble(
        x = c(.$x, .$xend * t_adjust_pos),
        y = c(.$y, .$yend * t_adjust_pos),
        z = c(.$z, .$zend * t_adjust_pos),
        text = c("", .$label)  # only put label at the end
      )
    }) %>%
    ungroup()
  
  # Build plot
  plot_ly() %>%
    add_trace(
      type = "scatter3d",
      mode = "markers",
      data = metrics_AF_std,
      x = ~.data[[x_pc]],
      y = ~.data[[y_pc]],
      z = ~.data[[z_pc]],
      color = ~gr,
      colors = gr_col,
      marker = list(size = 3, opacity = 0.8)
    ) %>%
    add_trace(
      type = "scatter3d",
      mode = "lines+text",
      data = arrows_long,
      x = ~x, y = ~y, z = ~z,
      text = ~text,
      textposition = "top center",
      line = list(width = 4, color = "black"),
      showlegend = FALSE
    ) %>%
    layout(
      scene = list(
        xaxis = list(title = x_lab),
        yaxis = list(title = y_lab),
        zaxis = list(title = z_lab),
        aspectmode = "cube"
      )
    )
}

