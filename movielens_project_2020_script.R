### The purpose of this script is to run the best algorithm (Algorithm 10) against the VALIDATION set
### The first step is to split VALIDATION into TRAIN and TEST sets
set.seed(755)

test_index <- createDataPartition(y = validation$rating, times = 1, p = 0.2,
                                  list = FALSE)

validation_train_set <- validation[-test_index,]

validation_test_set <- validation[test_index,]

validation_test_set <- validation_test_set %>%
  semi_join(validation_train_set, by = "movieId") %>%
  semi_join(validation_train_set, by = "userId")

mu <- mean(validation_train_set$rating)

### The second step is to use Algorithm 10 to predict ratings

movie_reg_avgs <- validation_train_set %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda03), n_i = n())

user_reg_avgs <- validation_train_set %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu)/(n()+lambda05), n_i = n())

predicted_ratings_validation <- validation_test_set %>%
  left_join(movie_reg_avgs, by='movieId') %>%
  left_join(user_reg_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

### The third step is to calculate the RMSE

RMSE_VAL <- RMSE(predicted_ratings_validation, validation_test_set$rating)

### The final step is to print the RMSE

RMSE_VAL

