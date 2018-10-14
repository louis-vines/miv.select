binned_feature <- bin_numeric(german_credit_data, x = "credit_amount")

test_that("bin_numeric creates an object of class binned_numeric", {
  expect_is(binned_feature, "binned_numeric")
  expect_is(binned_feature, "binned")
})

test_that("bin_numeric creates bins based on a supervised binning alg", {
  expected_names <- c("feature", "feature_type", "significant_splits_found",
                      "cuts", "levels", "tree", "iv", "iv_table")

  expect_equal(names(binned_feature), expected_names)
  expect_known_value(binned_feature, "references/bin_numeric/supervised_binned.RDS")
})

test_that("bin_numeric can accept different names for y variable", {
  german_data_different_y <- german_credit_data %>% rename(y = gb12)

  binned_feature <- bin_numeric(german_data_different_y, x = "credit_amount", y = "y")
  expect_known_value(binned_feature, "references/bin_numeric/supervised_binned_new_y.RDS")
})

test_that("if no significant splits are found, this is reported", {
  x <- "existing_credits"

  binned_feature <- bin_numeric(german_credit_data, x = x)
  expected_output <- list(
    feature = x,
    feature_type = "numeric",
    significant_splits_found = FALSE
  )

  expect_equal(binned_feature, expected_output)
})

test_that("tree_control can be used to pass parameters to ctree algorithm used for binning", {
  binned_feature <- bin_numeric(german_credit_data, x = "credit_amount")

  bin_with_small_tree <- create_numeric_supervised_bins(
    german_credit_data,
    x = "credit_amount",
    tree_control = ctree_control(maxdepth = 1)
  )

  expect_gt(length(binned_feature$tree), length(bin_with_small_tree$tree))
})

test_that("if a bins argument is provided, bin_numeric creates specified number of bins
           of roughly the same frequency", {
  #example 1
  n_bins <- 5

  binned_feature <- bin_numeric(german_credit_data, x = "credit_amount", bins = n_bins)
  binned_iv_table <- binned_feature$iv_table
  bins_are_all_roughly_equal_check <- all(
    round(binned_iv_table$freq / 1000, 2) == ((sum(binned_iv_table$freq) / n_bins) / 1000)
  )

  expect_equal(length(binned_feature$levels), n_bins)
  expect_true(bins_are_all_roughly_equal_check)

  #example 2
  n_bins <- 10

  binned_feature <- bin_numeric(german_credit_data, x = "credit_amount", bins = n_bins)

  expect_equal(length(binned_feature$levels), n_bins)
})

test_that("if there are less unique values than bins requested, create_numeric_frequency_bins
           uses the unique values as cutpoints", {
  #the feature being binned takes four unique values
  binned_feature <- bin_numeric(german_credit_data, x = "installment_rate_income", bins = 5)
  binned_groups <- binned_feature$iv_table$group
  expected_groups <- c("(,1]", "(1,2]", "(2,3]", "(3,]") %>% {factor(., levels = .)}

  expect_equal(binned_groups, expected_groups)
})

test_that("if bins is not an integer >= 2 an error is raised", {
  error_regex <- "must be integer"
  expect_error(bin_numeric(feature_to_bin, bins=1), regexp=error_regex)
  expect_error(bin_numeric(feature_to_bin, bins=-1), regexp=error_regex)
  expect_error(bin_numeric(feature_to_bin, bins=3.2), regexp=error_regex)
})

describe("predict.binned_numeric()", {
  feature_to_bin <- "duration"
  binned_feature <- bin_numeric(german_credit_data, x = feature_to_bin)

  it("bins the relevant features with the largest category in the factor as the first level", {
    df_with_binned_feature <- predict(binned_feature, german_credit_data)

    binned_feature_levels <- levels(df_with_binned_feature$duration)
    expected_levels <- c("(11,33]", "(,11]", "(33,]")

    expect_equal(binned_feature_levels, expected_levels)
  })

  describe("when largest_level_first is false", {
    it("bins the relevant feature column with the levels in ascending order", {
      df_with_binned_feature <- predict(binned_feature, german_credit_data, largest_level_first = FALSE)
      expected_levels <- c("(,11]", "(11,33]", "(33,]")

      expect_true(all(df_with_binned_feature[[feature_to_bin]] %in% binned_feature$levels))
    })
  })
})

test_that("binned_numeric objects has a plot method that creates a graph with 3 elements", {
  binned_feature <- bin_numeric(german_credit_data, x = "credit_amount")
  binned_feature_plot <- plot(binned_feature)

  expect_equal(length(binned_feature_plot$layers), 3)
  purrr::walk(binned_feature_plot$layers, ~ expect_is(.x, "ggproto"))
})

test_that("if the original data set is also passed to the plot method the plot has 4 elements", {
  binned_feature <- bin_numeric(german_credit_data, x = "credit_amount")
  binned_feature_plot <- plot(binned_feature, german_credit_data)

  expect_equal(length(binned_feature_plot$layers), 4)
  purrr::walk(binned_feature_plot$layers, ~ expect_is(.x, "ggproto"))
})
