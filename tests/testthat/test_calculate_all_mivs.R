credit_data_small <- german_credit_data %>%
  select(ca_status, credit_history, purpose, gb12)

candidate_model <- glm(gb12 ~ ca_status, data = credit_data_small,
                       family = binomial)

credit_data_w_pd <- credit_data_small %>%
  mutate(pd = predict(candidate_model, ., type = "response"))

test_that("given a data frame with a response variable named gb12
          and pd predictions from a previous model calculate_all_mivs
          calculates the marginal iv for all variables", {
  miv_output <- calculate_all_mivs(credit_data_w_pd)

  expected_output <- data_frame(
    feature_name = c("credit_history", "purpose", "ca_status"),
    iv = c(0.29323, 0.16919, 0.66601),
    miv = c(0.21541, 0.13165, 0)
  )

  expect_equal(miv_output, expected_output)
})
