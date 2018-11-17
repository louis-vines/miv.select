binned_feature <- bin_factor(german_credit_data, x = "property", y = "gb12")

supervised_ordered_binned_feature <- bin_factor(german_credit_data, x = "present_employment_since", y = "gb12", supervised = TRUE)

test_that("bin_factor creates an object of class binned_categorical", {
  expect_is(binned_feature, "binned_factor")
  expect_is(binned_feature, "binned")
})

test_that("if not specified, the algorithm treats each factor level as
           a seperate bin and presents data accordingly", {
  expected_names <- c("feature", "feature_type", "levels", "iv", "iv_table")

  expect_equal(names(binned_feature), expected_names)
  expect_known_output(binned_feature, "references/bin_factor/binned_feature.RDS")
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

  ordered_binned_feature <- bin_factor(german_data_w_ordered_col, x = "present_employment_since", y = "gb12")

  expect_equal(sort(ordered_binned_feature$iv_table$group), ordered_binned_feature$iv_table$group)
})

test_that("if supervised binning is requested then a supervised binning algorithm is used
           to group the factor levels", {
  supervised_binned_feature <- bin_factor(german_credit_data, x = "property", y = "gb12", supervised = TRUE)

  expected_names <- c("feature", "feature_type", "levels", "node_groups", "tree", "iv", "iv_table")

  expect_equal(names(supervised_binned_feature), expected_names)
  expect_known_output(supervised_binned_feature, "references/bin_factor/supervised_binned_feature.RDS")
})

test_that("tree_control can be used to pass parameters to ctree algorithm used for binning", {
  binned_feature <- bin_factor(german_credit_data, x = "property", y = "gb12", supervised = TRUE)

  bin_with_small_tree <- bin_factor(
    german_credit_data,
    x = "property",
    y = "gb12",
    supervised = TRUE,
    tree_control = ctree_control(maxdepth = 1)
  )

  expect_gt(length(binned_feature$tree), length(bin_with_small_tree$tree))
})

describe("predict.binned_factor()", {
  feature_to_bin <- "property"
    # we change the level names here as otherwise the largest_freq test passes without the
    # correct code implementation as "car or other" is alphabetically first so is first factor level
  german_credit_data <- german_credit_data %>%
  mutate(property = property %>% replace(. == "car or other", "xcar or other"))

  describe("when non-supervised binning is used to create the binned factor", {
    binned_feature <- bin_factor(german_credit_data, x = feature_to_bin, y = "gb12") 

    it("reorders the factor levels to have the largest category first", {
      data_with_binned_feature <- predict(binned_feature, german_credit_data)
      expected_factor_levels <- c("xcar or other", "real estate", "svngs. agrrement", "unknown/no")
      
      expect_equal(levels(data_with_binned_feature[[feature_to_bin]]), expected_factor_levels)
    })
  })

  describe("when supervised binning was used to create the binned_factor", {    
    binned_feature <- bin_factor(german_credit_data, x = feature_to_bin, y = "gb12", supervised = TRUE)

    it("applies the categorical groupings to the relevant column with the largest category as the first group", {
      data_with_binned_feature <- predict(binned_feature, german_credit_data)
      expected_factor_levels <- c("svngs. agrrement; xcar or other", "real estate", "unknown/no")
      
      expect_equal(levels(data_with_binned_feature[[feature_to_bin]]), expected_factor_levels)
    })
  })

  describe("largest_level_first is false", {
    binned_feature <- bin_factor(german_credit_data, x = feature_to_bin, y = "gb12") 
    data_with_binned_feature <- predict(binned_feature, german_credit_data, largest_level_first = FALSE)

    it("does not reorder the factor levels to have the largest first", {
      expected_factor_levels <- c("real estate", "svngs. agrrement", "unknown/no", "xcar or other")
      
      expect_equal(levels(data_with_binned_feature[[feature_to_bin]]), expected_factor_levels)
    })
  })
})

test_that("binned_factor objects have a predict method, if supervised binning was used for the binning
           process the category groupings is applied to the relevant column", {
  feature_to_bin <- "property"
  binned_feature <- bin_factor(german_credit_data, x = feature_to_bin, y = "gb12", supervised = TRUE)

  data_with_binned_feature <- predict(binned_feature, german_credit_data)
  binned_levels <- binned_feature$levels


  expect_true(all(data_with_binned_feature[[feature_to_bin]] %in% binned_levels))
})

test_that("the binned_factor predict method works with features other than property", {
  feature_to_bin <- "ca_status"
  binned_feature <- bin_factor(german_credit_data, x = feature_to_bin, y = "gb12", supervised = TRUE)

  data_with_binned_feature <- predict(binned_feature, german_credit_data)
  binned_levels <- binned_feature$levels

  expect_true(all(data_with_binned_feature[[feature_to_bin]] %in% binned_levels))
})

test_that("binned_factor objects have a plot method that creates a graph with 3 elements", {
  binned_feature <- bin_factor(german_credit_data, x = "property", y = "gb12")
  binned_feature_plot <- plot(binned_feature)

  expect_equal(length(binned_feature_plot$layers), 3)
  purrr::walk(binned_feature_plot$layers, ~ expect_is(.x, "ggproto"))
})

test_that("if the original data set is also passed to the plot method the plot has 4 elements", {
  binned_feature <- bin_factor(german_credit_data, x = "property", y = "gb12")
  binned_feature_plot <- plot(binned_feature, german_credit_data)

  expect_equal(length(binned_feature_plot$layers), 4)
  purrr::walk(binned_feature_plot$layers, ~ expect_is(.x, "ggproto"))
})
