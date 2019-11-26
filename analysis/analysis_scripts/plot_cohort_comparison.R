#' Plot Cohort Exposure Comparison
#' 
#' @param exposure_data      A data frame with (at minimum) the columns signature,
#'                           cohort, and exposure.

plot_cohort_comparison <- function(exposure_data, megabase_count = 3234.83) {
  exposure_data %>%
    mutate(
      signature = gsub('Signature ', 'Sig. ', signature),
      cohort = factor(cohort),
      exposure = exposure / megabase_count
    ) %>%
    ggplot(aes(
      x = cohort,
      y = exposure
    )) +
    geom_violin(aes(
      fill = signature,
      colour = signature
    )) +
    geom_jitter(width = 0.3, size = 0.2) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      strip.background = element_blank()
    ) +
    scale_fill_brewer(palette = 'Set1') +
    scale_colour_brewer(palette = 'Set1') +
    labs(
      y = 'Exposure\n(Muts / Mb)',
      colour = 'Signature',
      fill = 'Signature'
    ) +
    theme(
      axis.title = element_text(size = 11)
    )
}