data_for_binning <- data_frame(
    a = 1:3,
    b = 1:3,
    c = letters[1:3],
    gb12 = c(1, 0, 1)
)

test_that("bin_all creates binned_objects for corresponding feature data type", {
  example_data <- german_credit_data %>%
    select(ca_status, age, property, gb12)

  binned_objects <- bin_all(example_data, "gb12")
  binned_object_classes <- binned_objects %>%
    purrr::map_chr(~ class(.x)[1]) %>%
    setNames(NULL)

  expected_classes <- c("binned_factor", "binned_numeric", "binned_factor")

  expect_equal(binned_object_classes, expected_classes)  
})

test_that("variables can be excluded from the binning process", {
  with_mock(
    bin_factor = function(dframe, xvar, y, supervised, tree_control) "factor binning",
    bin_numeric = function(dframe, xvar, y, tree_control, bins) "numeric binning",
    {
      binned_objects <- bin_all(data_for_binning, "gb12", ,vars_to_exclude = c("b", "c"))
      expect_equal(names(binned_objects), "a")
    }
  )
})

test_that("user can specify that only certain variables are binned", {
  with_mock(
    bin_factor = function(dframe, xvar, y, supervised, tree_control) "factor binning",
    bin_numeric = function(dframe, xvar, y, tree_control, bins) "numeric binning",
    {
      binned_objects <- bin_all(data_for_binning, "gb12", vars_to_include = c("b", "c"))
      expect_equal(names(binned_objects), c("b", "c"))
    }
  )
})

test_that("the tree_control argument is passed to the atomic binning functions", {
  data_for_binning <- data_frame(
      a = 1:3,
      b = letters[1:3],
      gb12 = c(1, 0, 1)
  )

  control_val <- "tree control val:"
  bin_numeric_marker <- "numeric"
  bin_factor_marker <- "factor"

  expected_output <- list(
    a = paste(control_val, bin_numeric_marker),
    b = paste(control_val, bin_factor_marker)
  )

  with_mock(
    bin_factor = function(dframe, xvar, y, supervised, tree_control){
      paste(tree_control, bin_factor_marker)
    },
    bin_numeric = function(dframe, xvar, y,  tree_control, bins){
      paste(tree_control, bin_numeric_marker)
    },
    {
  
      binned_objects <- bin_all(data_for_binning, "gb12", tree_control = control_val)
      expect_equal(binned_objects, expected_output)
    }
  )
})

test_that("bin_all passes the bins function to bin_numeric", {
  data_for_binning <- data_frame(
      a = 1:3,
      gb12 = c(1, 0, 1)
  )

  n_bins <- 10

  with_mock(
    bin_numeric = function(dframe, xvar, y, tree_control, bins){
      bins
    },
    {
      binned_objects <- bin_all(data_for_binning, "gb12", bins = n_bins)
      expect_equal(binned_objects$a, n_bins)
    }
  )
})

test_that("if number of bins is specified in bin_all then categorical binning
           is done in unsupervied manner", {
  data_for_binning <- data_frame(
    a = letters[1:3],
    gb12 = c(1,0,1)
  )

  n_bins <- 10

  with_mock(
    bin_factor = function(dframe, x, y, supervised, tree_control){
      supervised
    },
    {
      binned_objects <- bin_all(data_for_binning, "gb12", bins = n_bins)
      categorical_binning_was_supervised <- binned_objects$a
      expect_false(categorical_binning_was_supervised)
    }
  )
})

test_that("if number of bins is specified in bin_all then categorical binning
           is supervied manner", {
  data_for_binning <- data_frame(
    a = letters[1:3],
    gb12 = c(1,0,1)
  )

  with_mock(
    bin_factor = function(dframe, x, y, supervised, tree_control){
      supervised
    },
    {
      binned_objects <- bin_all(data_for_binning, "gb12")
      categorical_binning_was_supervised <- binned_objects$a
      expect_true(categorical_binning_was_supervised)
    }
  )
})


test_that("bin_all prints which variables are being binned if verbose = TRUE", {
  data_for_binning <- data_frame(
    a = 1:3,
    b = 1:3,
    gb12 = c(1,0,1)
  )

  expected_message <- "binning: a ...\nbinning: b"

  with_mock(
    bin_numeric = function(dframe, xvar, y, tree_control, bins) "numeric binning",
    {
      expect_output(bin_all(data_for_binning, "gb12", verbose = TRUE), regexp = expected_message)
    }
  )
})
