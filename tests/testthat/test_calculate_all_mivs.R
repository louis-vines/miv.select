credit_data_small <- german_credit_data %>%
  select(ca_status, credit_history, purpose, gb12)

candidate_model <- glm(gb12 ~ ca_status, data = credit_data_small,
                       family = binomial)

credit_data_w_pd <- credit_data_small %>%
  mutate(pd = predict(candidate_model, ., type = "response"))

test_that("given a data frame with a response variable named gb12
           and pd predictions from a previous model,
           calculate_all_mivs calculates the marginal iv for all variables", {
  miv_output <- calculate_all_mivs(credit_data_w_pd, y = "gb12")

  expected_output <- data_frame(
    feature_name = c("credit_history", "purpose", "ca_status"),
    iv = c(0.29323, 0.16919, 0.66601),
    miv = c(0.21541, 0.13165, 0)
  )

  expect_equal(miv_output, expected_output)
})

test_that("the feature data types can be factor or character", {
  #current data types of features are all character
  #so we need to test a factor variable
  credit_data_w_pd$purpose <- factor(credit_data_w_pd$purpose)

  expect_error(calculate_all_mivs(credit_data_w_pd, y = "gb12"), NA)
})

test_that("if numeric variables are included an error is thrown", {
  numeric_col <- german_credit_data %>% select(duration)

  credit_data_w_pd <- bind_rows(credit_data_w_pd, numeric_col)

  expect_error(
    calculate_all_mivs(credit_data_w_pd, y = "gb12"),
    "cannot calculate miv for a non-categorical variable"
  )
})

test_that("if as_dframe = FALSE a detailed output of the miv calculations
           is presented in list form", {
  miv_output <- calculate_all_mivs(credit_data_w_pd, y = "gb12", as_dframe = FALSE)
  expected_output_names <- c("actual_woe", "expected_woe",
                             "miv_table", "iv", "miv")

  purrr::walk(miv_output, function(output_element) {
    expect_equal(names(output_element), expected_output_names)
  })
})

test_that("user can specify which variables are excluded in the calculation", {
  miv_output <- calculate_all_mivs(credit_data_w_pd, y = "gb12", vars_to_exclude = "purpose")
  expected_features_included <- c("credit_history", "ca_status")

  expect_equal(miv_output$feature_name, expected_features_included)
})

test_that("user can specify which variables are included in the calculation", {
  miv_output <- calculate_all_mivs(credit_data_w_pd, y = "gb12", vars_to_include = c("ca_status", "purpose"))

  expected_features_included <- c("purpose", "ca_status")

  expect_equal(miv_output$feature_name, expected_features_included)
})
