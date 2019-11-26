plot_exposures <- function(exposures_df, title = NULL) {
  exposures_table <- exposures_df %>%
    mutate(signature = gsub('Signature ', '', signature))
  
  exposures_matrix <- exposures_table %>%
    select(sample, signature, normalized_exposure) %>%
    spread(signature, normalized_exposure)
  
  clust <- exposures_matrix %>%
    select(-sample) %>%
    as.matrix %>%
    dist %>%
    hclust
  
  order <- clust$order
  sample_levels <- exposures_matrix$sample[order]  
  
  exposures_heatmap <- exposures_table %>%
    mutate(sample = factor(sample, levels = sample_levels)) %>%
    group_by(sample) %>%
    mutate(normalized_exposure = exposure / sum(exposure)) %>%
    ungroup() %>%
    ggplot(aes(
      x = signature,
      y = sample,
      fill = normalized_exposure
    )) +
    geom_tile() +
    scale_fill_viridis() +
    theme(
      axis.text.y = element_blank(),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank()
    )
  
  if (! is.null(title)) {
    exposures_heatmap <- exposures_heatmap + ggtitle(title)
  }
  
  burden_plot <- exposures_table %>%
    group_by(sample) %>%
    summarise(mutation_burden = sum(exposure) / MEGABASE_COUNT) %>%
    ungroup() %>%
    mutate(sample = factor(sample, levels = sample_levels)) %>%
    ggplot(aes(
      y = mutation_burden,
      x = sample
    )) +
    labs(
      y = 'Mutations/Mb)'
    ) +
    coord_flip() +
    geom_bar(stat = 'identity') +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  plot_grid(
    exposures_heatmap + theme(legend.position = 'none'),
    burden_plot,
    nrow = 1,
    align = 'h',
    rel_widths = c(2,1)
  )
}