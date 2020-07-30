### The purpose of this script is to run the best algorithm (Algorithm 10) against the VALIDATION set

### The first step was to load the necessary libraries from the repository http://cran.us.r-project.org. 

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")

if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

### The second step entailed loading and wrangling the MovieLens set from http://files.grouplens.org/datasets/movielens/ml-10m.zip

l <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                            title = as.character(title),
                                            genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# The third step called for generating the VALIDATION set from 10% of MovieLens data

set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

validation <- temp %>% 
      semi_join(edx, by = "movieId") %>%
      semi_join(edx, by = "userId")

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

### The fourth step was to split VALIDATION into TRAIN and TEST sets

set.seed(755)

test_index <- createDataPartition(y = validation$rating, times = 1, p = 0.2,
                                  list = FALSE)

validation_train_set <- validation[-test_index,]

validation_test_set <- validation[test_index,]

validation_test_set <- validation_test_set %>%
  semi_join(validation_train_set, by = "movieId") %>%
  semi_join(validation_train_set, by = "userId")

mu <- mean(validation_train_set$rating)

### The fifth step was to use Algorithm 10 to predict ratings

lambda03 <- 2.25

lambda05 <- 5.5

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

### The sixth step is to calculate the RMSE

RMSE_VAL <- RMSE(predicted_ratings_validation, validation_test_set$rating)

### The final step is to print the RMSE

RMSE_VAL

