library(dplyr)

data(train, package="Information")
data(valid, package="Information")

purchase_train <- train %>%
  as_data_frame() %>%
  filter(TREATMENT == 1) %>%
  select(-TREATMENT)

purchase_test <- valid %>%
  as_data_frame() %>%
  filter(TREATMENT == 1) %>%
  select(-TREATMENT)

save(purchase_train, file = "data/purchase_train.rda")
save(purchase_test, file = "data/purchase_test.rda")
