suppressMessages(library(dplyr))
suppressMessages(library(readr))

ca_status_levels <- c("No Acc.", "(;0DM)", "<0DM;200DM)", "<200DM;)")

grouped_ca_status <- read_csv(
  "fixtures/binned_ca_status.csv",
  col_types = cols(ca_status = col_factor(levels = ca_status_levels),
                   gb12 = col_integer(),
                   pd = col_number())
) %>%
  mutate(ca_status = factor(ca_status, ordered = TRUE)) %>%
  rename(group = ca_status)

test_that("given a dataframe with a column called group containing a categorical variable
           and a column column entitled gb12 response, build_iv_table will costruct
           a table calculating information values", {
  iv_table <- build_iv_table(grouped_ca_status)
  expected_iv_table <- tibble::tribble(
           ~group, ~freq, ~freq_bad, ~freq_good, ~prop_total, ~bad_rate,   ~odds, ~log_odds,     ~woe,     ~iv,
        "No Acc.",   394,        46,        348,       0.394,   0.11675, 0.13218,  -2.02356, -1.17626, 0.40441,
         "(;0DM)",   274,       135,        139,       0.274,   0.49270, 0.97122,  -0.02920,  0.81810, 0.20569,
    "<0DM;200DM)",   269,       105,        164,       0.269,   0.39033, 0.64024,  -0.44591,  0.40139, 0.04645,
       "<200DM;)",    63,        14,         49,       0.063,   0.22222, 0.28571,  -1.25276, -0.40547, 0.00946
  ) %>%
    mutate(group = factor(group, levels = grouped_ca_status$group %>% levels))

  expect_equal(iv_table, expected_iv_table)
})

test_that("if some data is missing from the group feature, this is reported as a category called '<missing>'
           which is the bottom row of the data", {
  grouped_ca_status$group[1:100] <- NA

  iv_table <- build_iv_table(grouped_ca_status)
  expected_iv_table <- tibble::tribble(
           ~group, ~freq, ~freq_bad, ~freq_good, ~prop_total, ~bad_rate,    ~odds, ~log_odds,     ~woe,     ~iv,
        "No Acc.",   359,        43,        316,       0.359,   0.11978,  0.13608,  -1.99454, -1.14724, 0.35346,
         "(;0DM)",   248,       125,        123,       0.248,   0.50403,  1.01626,   0.01613,  0.86343, 0.20804,
    "<0DM;200DM)",   236,        94,        142,       0.236,   0.39831,  0.66197,  -0.41253,  0.43477, 0.04803,
       "<200DM;)",    57,        13,         44,       0.057,   0.22807,  0.29545,  -1.21924, -0.37194, 0.00726,
      "(Missing)",   100,        25,         75,       0.100,   0.25000,  0.33333,  -1.09861, -0.25131, 0.00598
  ) %>%
    mutate(group = factor(group, levels = c(levels(grouped_ca_status$group), "(Missing)")))

  expect_equal(iv_table, expected_iv_table)
})

test_that("a different name can be provided for the response variable", {
  grouped_ca_status <- grouped_ca_status %>%
    rename(y = gb12)

  iv_table <- build_iv_table(grouped_ca_status, y = "y")
  expected_iv_table <- tibble::tribble(
           ~group, ~freq, ~freq_bad, ~freq_good, ~prop_total, ~bad_rate,   ~odds, ~log_odds,     ~woe,     ~iv,
        "No Acc.",   394,        46,        348,       0.394,   0.11675, 0.13218,  -2.02356, -1.17626, 0.40441,
         "(;0DM)",   274,       135,        139,       0.274,   0.49270, 0.97122,  -0.02920,  0.81810, 0.20569,
    "<0DM;200DM)",   269,       105,        164,       0.269,   0.39033, 0.64024,  -0.44591,  0.40139, 0.04645,
       "<200DM;)",    63,        14,         49,       0.063,   0.22222, 0.28571,  -1.25276, -0.40547, 0.00946
  ) %>%
    mutate(group = factor(group, levels = grouped_ca_status$group %>% levels))

  expect_equal(iv_table, expected_iv_table)
})
