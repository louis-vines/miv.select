test_that("remove_x_axis is a function to remove x axis elements from a ggplot object", {
  expected_object <-  theme(axis.text.x = element_blank(),
                             axis.ticks.x = element_blank(),
                             axis.title.x = element_blank(),
                             axis.line.x = element_blank())

  expect_identical(remove_x_axis(), expected_object)
})
