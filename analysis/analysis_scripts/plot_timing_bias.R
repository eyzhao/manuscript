#' Plot Signature Timing Bias
#' 
#' @param timing_data        A data frame with (at minimum) the columns met_signature,
#'                           cohort, exposure, and timing.

plot_timing_bias <- function(timing_data, megabase_count = 3234.83) {
  timing_data %>%
    separate(raw_signature, c('raw_signature', 'cohort')) %>%
    mutate(
      exposure = exposure / megabase_count,
      met_signature = gsub('Signature ', 'Sig. ', met_signature),
      cohort_signature = sprintf('%s, %s', cohort, met_signature),
      timing = case_when(
        n_populations == 1 & min_prevalence > 0.9 ~ rnorm(n = n(), mean = -0.25, sd = 0.025),
        n_populations == 1 ~ Inf,
        TRUE ~ timing
      )
    ) %>%
    filter(! is.infinite(timing)) %>%
    ggplot(aes(
      x = exposure,
      y = timing,
      colour = cohort_signature
    )) +
    geom_point(size = 0.9) +
    scale_colour_brewer(palette = 'Set1') +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    scale_y_continuous(
      limits = c(-0.35,1),
      breaks = seq(-0.25, 1, 0.25),
      labels = c('Single\nPopulation', '(Early) 0', '0.25', '0.5', '0.75', '(Late) 1')
    ) +
    geom_hline(yintercept = -0.125) +
    labs(
      x = 'Exposure (Muts/Mb)',
      y = 'Timing',
      colour = 'Cohort'
    )
}