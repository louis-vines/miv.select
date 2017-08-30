#' @import dplyr
build_iv_table <- function(dframe, y = "gb12"){
  total_goods <- sum(dframe[[y]] == 0)
  total_bads <- sum(dframe[[y]] == 1)
  total_log_odds <- log(total_bads / total_goods)

  iv_table <- wrapr::let(list(.y. = y), {
    dframe %>%
      group_by(group) %>%
      summarise(freq = n(),
                freq_bad = sum(.y. == 1),
                freq_good = sum(.y. == 0)) %>%
      mutate(prop_total = freq / sum(freq),
             bad_rate = freq_bad / freq,
             odds = freq_bad / freq_good,
             log_odds = log(odds),
             woe = log_odds - total_log_odds,
             iv = (freq_bad/total_bads - freq_good/total_goods) * woe) %>%
      mutate_if(is.numeric, ~ round(.x, 5))
  }) %>%
    mutate(group = forcats::fct_explicit_na(group))

  attr(iv_table$group, "class") <- "factor"

  iv_table
}
