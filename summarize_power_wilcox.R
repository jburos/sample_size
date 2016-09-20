
library(tidyverse)

alt_mannwhitney <- function(x_benefit, x_non) {
  wilcox.test(x ~ group, data = df)
}


sim_data <- function(index = 1, 
                     n = 30,
                     loc_benefit = 12.4,
                     loc_non = 6.4,
                     prop_benefit = 0.2
) {
  n_benefit = n*prop_benefit
  n_non = n*(1-prop_benefit)
  sim_f = function(n, loc) exp(rnorm(n = n, mean = log(loc), sd = log(loc)))
  x_benefit <- sim_f(n = n_benefit, loc = loc_benefit)
  x_non <- sim_f(n = n_non, loc = loc_non)
  df <- tbl_df(list(x = x_benefit, group = 'benefit')) %>%
    bind_rows(tbl_df(list(x = x_non, group = 'non')))
  df
}

res <- seq_len(1000) %>%
  map(sim_data, loc_benefit = 12.4, loc_non = 6.4, n = 30, prop_benefit = 0.19) %>%
  map(~ wilcox.test(x ~ group, data = .)) %>%
  map_df(broom::tidy)

## power to detect effect at alpha 0.05
mean(res$p.value < 0.05)

## plot distribution of p-values from simulations
sum_res <- res %>%
  dplyr::group_by(`p.value`) %>%
  dplyr::summarize(n_pvalue = n()) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(`p.value`) %>%
  dplyr::mutate(total_n = sum(n_pvalue),
                cum_n = cumsum(n_pvalue),
                power = cum_n/total_n
  ) %>%
  dplyr::rename(alpha = `p.value`)

ggplot(sum_res, aes(x = alpha, y = power)) + 
  geom_line() + 
  geom_vline(aes(xintercept=0.05), color = 'green', linetype = 'dashed') +
  theme_minimal() +
  ggtitle('Power to detect assoc of mutation load with DCB\nAssuming n = 30, prop = 0.3, and medians = 12.4 vs 6.4')
