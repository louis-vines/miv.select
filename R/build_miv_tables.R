build_miv_tables <- function(dframe, y, predictions){
  dframe$.predictions. <- predictions

  total_goods <- sum(dframe[[y]] == 0)
  total_bads <- sum(dframe[[y]] == 1)
  total_log_odds <- log(total_bads / total_goods)

  actual_woe <- build_iv_table(dframe, y = y) %>%
    select(-iv)

  expected_woe <- dframe %>%
    group_by(group) %>%
    summarise(freq = n(),
              freq_bad = sum(.predictions.),
              freq_good = sum(1 - (.predictions.))) %>%
    mutate(prop_total = freq / sum(freq),
           bad_rate = freq_bad / freq,
           odds = freq_bad / freq_good,
           log_odds = log(odds),
           woe = log_odds - total_log_odds) %>%
    mutate_if(is.numeric, ~ round(.x, 5)) %>%
    mutate(group = forcats::fct_explicit_na(group))

  attr(expected_woe$group, "class") <- "factor"

  miv_table <- actual_woe %>%
    select(group, freq_bad, freq_good, actual_woe = woe) %>%
    left_join(expected_woe %>% select(group, expected_woe = woe),
              by = 'group') %>%
    mutate(iv = (freq_bad/total_bads - freq_good/total_goods) * actual_woe) %>%
    mutate(miv = (freq_bad/total_bads - freq_good/total_goods) * (actual_woe - expected_woe)) %>%
    mutate(iv = round(iv, 5)) %>%
    mutate(miv = round(miv, 5))

  ivs_contain_inf <- is.infinite(c(miv_table$iv, miv_table$miv)) %>% any

  if(ivs_contain_inf){
    warning("replacing Inf with 0 in an iv/miv calculation, this is probably because there was a small group with only one class")
    miv_table <- miv_table %>%
      mutate_at(c('iv', 'miv'), ~ replace(.x, is.infinite(.x), 0))
  }

  list(actual_woe = actual_woe, expected_woe = expected_woe, miv_table = miv_table)
}
