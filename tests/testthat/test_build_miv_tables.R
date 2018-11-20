ca_status_levels <- c("No Acc.", "(;0DM)", "<0DM;200DM)", "<200DM;)")

grouped_ca_status <- readr::read_csv(
  "fixtures/binned_ca_status.csv",
  col_types = readr::cols(ca_status = readr::col_factor(levels = ca_status_levels),
                   gb12 = readr::col_integer(),
                   pd = readr::col_number())
) %>%
  mutate(ca_status = factor(ca_status, ordered = TRUE)) %>%
  rename(group = ca_status)

# we've stored the fixture with the predictions included for ease
# they need to be removed from the df and passed in to the miv_function as their own dataframe
predictions <- grouped_ca_status$pd
grouped_ca_status <- grouped_ca_status %>% select(-pd)


actual_woe_table_expected <- tibble::tribble(
         ~group, ~freq, ~freq_bad, ~freq_good, ~prop_total, ~bad_rate,   ~odds, ~log_odds,     ~woe,
      "No Acc.",   394,        46,        348,       0.394,   0.11675, 0.13218,  -2.02356, -1.17626,
       "(;0DM)",   274,       135,        139,       0.274,   0.49270, 0.97122,  -0.02920,  0.81810,
  "<0DM;200DM)",   269,       105,        164,       0.269,   0.39033, 0.64024,  -0.44591,  0.40139,
     "<200DM;)",    63,        14,         49,       0.063,   0.22222, 0.28571,  -1.25276, -0.40547
) %>%
  mutate(group = factor(group, levels = grouped_ca_status$group %>% levels))

expected_woe_table_expected <- tibble::tribble(
         ~group, ~freq, ~freq_bad, ~freq_good, ~prop_total, ~bad_rate,   ~odds, ~log_odds,     ~woe,
      "No Acc.",   394, 107.34628,  286.65372,       0.394,   0.27245, 0.37448,  -0.98222, -0.13492,
       "(;0DM)",   274,  86.96693,  187.03307,       0.274,   0.31740, 0.46498,  -0.76576,  0.08154,
  "<0DM;200DM)",   269,  86.89817,  182.10183,       0.269,   0.32304, 0.47720,  -0.73983,  0.10747,
     "<200DM;)",    63,  18.78862,   44.21138,       0.063,   0.29823, 0.42497,  -0.85573, -0.00843
) %>%
  mutate(group = factor(group, levels = grouped_ca_status$group %>% levels))

miv_table_expected <- tibble::tribble(
         ~group, ~freq_bad, ~freq_good, ~actual_woe, ~expected_woe,     ~iv,    ~miv,
      "No Acc.",        46,        348,    -1.17626,      -0.13492, 0.40441, 0.35802,
       "(;0DM)",       135,        139,     0.81810,       0.08154, 0.20569, 0.18519,
  "<0DM;200DM)",       105,        164,     0.40139,       0.10747, 0.04645, 0.03401,
     "<200DM;)",        14,         49,    -0.40547,      -0.00843, 0.00946, 0.00926
) %>%
  mutate(group = factor(group, levels = grouped_ca_status$group %>% levels))

test_that("given a dataframe with a column called group containing a categorical variable,
           a response variable entitled gb12 and a column entitled pd containing
           pds from a previous model description, build_miv_tables will calculate
           partial miv values", {
  miv_tables <- build_miv_tables(grouped_ca_status, y = "gb12", predictions)

  expect_equal(names(miv_tables), c("actual_woe", "expected_woe", "miv_table"))
  expect_equal(miv_tables$actual_woe, actual_woe_table_expected)
  expect_equal(miv_tables$expected_woe, expected_woe_table_expected)
  expect_equal(miv_tables$miv_table, miv_table_expected)
})

test_that("if some data is missing from the group feature, build_miv_tables creates a category called '(missing)'
           which is the bottom row of the data", {
  grouped_ca_status$group[1:100] <- NA


  miv_tables <- build_miv_tables(grouped_ca_status, y = "gb12", predictions)
  expected_table_categories <- c("No Acc.", "(;0DM)", "<0DM;200DM)", "<200DM;)", "(Missing)") %>%
    factor(., levels = .)

  expect_equal(miv_tables$actual_woe$group, expected_table_categories)
  expect_equal(miv_tables$expected_woe$group, expected_table_categories)
  expect_equal(miv_tables$miv_table$group, expected_table_categories)
})

test_that("build_miv_tables can accept different names for the response column", {
  grouped_ca_status <- grouped_ca_status %>% rename(y = gb12)

  miv_tables <- build_miv_tables(grouped_ca_status, y = "y", predictions)

  expect_equal(miv_tables$actual_woe, actual_woe_table_expected)
  expect_equal(miv_tables$expected_woe, expected_woe_table_expected)
  expect_equal(miv_tables$miv_table, miv_table_expected)
})
