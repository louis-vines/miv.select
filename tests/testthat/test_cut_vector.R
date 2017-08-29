x <- c(2, 4, 8)

test_that('cut_vector cuts a vector but takes -Inf, Inf as top and bottom limits', {
  ##example_1-------------------------------------
  breaks <- c(3, 5)
  expected_cut_vector <- factor(
    c("(,3]", "(3,5]", "(5,]"),
    levels = c("(,3]", "(3,5]", "(5,]")
  )

  expect_equal(cut_vector(x, breaks = breaks), expected_cut_vector)

  ##example_2-------------------------------------
  breaks <- 4
  expected_cut_vector <- factor(
    c("(,4]", "(,4]", "(4,]"),
    levels = c("(,4]", "(4,]")
  )

  expect_equal(cut_vector(x, breaks = breaks), expected_cut_vector)
})

test_that('if more than one of the same break point is given it is ignored', {
  ##example_1-------------------------------------
  breaks <- c(3, 3, 5)
  expected_cut_vector <- factor(
    c("(,3]", "(3,5]", "(5,]"),
    levels = c("(,3]", "(3,5]", "(5,]")
  )

  expect_equal(cut_vector(x, breaks = breaks), expected_cut_vector)

  ##example_2-------------------------------------
  breaks <- c(-Inf, 4, Inf)
  expected_cut_vector <- factor(
    c("(,4]", "(,4]", "(4,]"),
    levels = c("(,4]", "(4,]")
  )

  expect_equal(cut_vector(x, breaks = breaks), expected_cut_vector)
})

test_that('if a group has zero observations it is still included in the levels', {
  x <- c(0, 1, 5, 10)
  breaks <- c(2, 4)

  expected_cut_vector <- factor(
    c("(,2]", "(,2]", "(4,]", "(4,]"),
    levels = c("(,2]", "(2,4]", "(4,]")
  )

  expect_equal(cut_vector(x, breaks), expected_cut_vector)
})

test_that('by default intervals are closed on the right', {
  x <- c(2, 4, 6, 8, 8.1)
  breaks <- c(2, 4, 8)

  expected_cut_vector <- factor(
    c("(,2]", "(2,4]", "(4,8]", "(4,8]", "(8,]"),
    levels = c("(,2]", "(2,4]", "(4,8]", "(8,]")
  )

  expect_equal(cut_vector(x, breaks), expected_cut_vector)
})

test_that('if closed_on_right is false then intervals are closed on the left instead', {
  x <- c(1, 2, 4, 6, 8, 8.1)
  breaks <- c(2, 4, 8)

  x_cut <- cut_vector(x, breaks, closed_on_right = FALSE)
  expected_x_cut <- factor(
    c("[,2)", "[2,4)", "[4,8)", "[4,8)", "[8,)", "[8,)"),
    levels = c("[,2)", "[2,4)", "[4,8)", "[8,)")
  )

  expect_equal(x_cut, expected_x_cut)
})
