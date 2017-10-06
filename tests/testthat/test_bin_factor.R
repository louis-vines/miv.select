binned_feature <- bin_factor(german_credit_data, x = "property")

supervised_ordered_binned_feature <- bin_factor(german_credit_data, x = "present_employment_since", supervised = TRUE)

test_that("bin_factor creates an object of class binned_categorical", {
  expect_is(binned_feature, "binned_factor")
  expect_is(binned_feature, "binned")
})

test_that("if not specified, the algorithm treats each factor level as
           a seperate bin and presents data accordingly", {
  expected_names <- c("feature", "feature_type", "levels", "iv", "iv_table")

  expect_equal(names(binned_feature), expected_names)
  expect_equal_to_reference(binned_feature, "references/bin_factor/binned_feature.RDS")
})

test_that("if the feature that was binned isn't an ordered factor,
           the iv_table is ordered by the woe", {
  expect_equal(sort(binned_feature$iv_table$woe), binned_feature$iv_table$woe)
})

test_that("if the feature that was binned is an ordered factor,
           the iv_table is ordered by the factor levels", {
  employment_status_levels <- c("unemployed", "(;1)", "<1;4)", "<4;7)", "<7;)")
  german_data_w_ordered_col <- german_credit_data %>%
    mutate(present_employment_since = factor(present_employment_since,
                                              levels = employment_status_levels,
                                              ordered = TRUE))

  ordered_binned_feature <- bin_factor(german_data_w_ordered_col, x = "present_employment_since")

  expect_equal(sort(ordered_binned_feature$iv_table$group), ordered_binned_feature$iv_table$group)
})

test_that("if supervised binning is requested then a supervised binning algorithm is used
           to group the factor levels", {
  supervised_binned_feature <- bin_factor(german_credit_data, x = "property", supervised = TRUE)

  expected_names <- c("feature", "feature_type", "levels", "node_groups", "tree", "iv", "iv_table")

  expect_equal(names(supervised_binned_feature), expected_names)
  expect_equal_to_reference(supervised_binned_feature, "references/bin_factor/supervised_binned_feature.RDS")
})

test_that("tree_control can be used to pass parameters to ctree algorithm used for binning", {
  binned_feature <- bin_factor(german_credit_data, x = "property", supervised = TRUE)

  bin_with_small_tree <- bin_factor(
    german_credit_data,
    x = "property",
    supervised = TRUE,
    tree_control = ctree_control(maxdepth = 1)
  )

  expect_gt(length(binned_feature$tree), length(bin_with_small_tree$tree))
})

test_that("binned_factor objects have a predict method, if supervised binning was not used for the binning
           process the transformed dataframe is the same as the original dataframe", {
  data_with_binned_feature <- predict(binned_feature, german_credit_data)

  expect_equal(german_credit_data, data_with_binned_feature)
})

test_that("binned_factor objects have a predict method, if supervised binning was used for the binning
           process the transformed dataframe is the same as the original dataframe", {
  feature_to_bin <- "property"
  binned_feature <- bin_factor(german_credit_data, x = feature_to_bin, supervised = TRUE)

  data_with_binned_feature <- predict(binned_feature, german_credit_data)
  expected_factor_levels <- c("real estate", "car or other; svngs. agrrement", "unknown/no")


  expect_true(all(data_with_binned_feature[[feature_to_bin]] %in% expected_factor_levels))
})
