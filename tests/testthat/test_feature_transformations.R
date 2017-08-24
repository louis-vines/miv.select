test_that('replace_na replaces NA values with provided replacement', {
  expect_equal(replace_na(c(1, 2, NA), 0), c(1, 2, 0))
  expect_equal(replace_na(c('a', NA), 'b'), c('a', 'b'))
  expect_equal(replace_na(c(NA), TRUE), TRUE)
  expect_equal(replace_na(c(1, 2, NA), NA), c(1, 2, NA))
})

test_that('replace_na leaves a vector unchanged if no NA values are present', {
  expect_equal(replace_na(1:3, 10), 1:3)
  expect_equal(replace_na('a', 1), 'a')
})

test_that('safe_log takes the log of a function adjusted by the adjuster value',{
  #default adjuster = 1
  expect_equal(safe_log(0:2), log(1:3))
  expect_equal(safe_log(10:12, 2), log(10:12 + 2))
})
