test_that("remove_x_axis is a function to remove x axis elements from a ggplot object", {
  expected_object <-  theme(axis.text.x = element_blank(),
                             axis.ticks.x = element_blank(),
                             axis.title.x = element_blank(),
                             axis.line.x = element_blank())

  expect_identical(remove_x_axis(), expected_object)
})

test_that("gg_value_labels can be added to ggplot objects without throwing
           an exception", {
  example_data <- data_frame(
    x = (-5:5) / 10,
    y =  x^3
  )

  example_plot <- ggplot(example_data, aes(x, y)) +
    geom_bar(stat = "identity")

  expect_error(example_plot + gg_value_labels(example_data, "y"),
               regexp = NA)

  expect_error(example_plot + gg_value_labels(example_data, "y", size = 1.5),
               regexp = NA)
})
