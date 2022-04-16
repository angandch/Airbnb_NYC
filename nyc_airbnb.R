if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(recosystem)

# This data set was imported from Kaggle. It is available at:
# https://www.kaggle.com/dgomonov/new-york-city-airbnb-open-data
airbnb_nyc <- read_csv("./airbnb_nyc.csv")

# This data set contains information about airbnbs available in 2019.

# We are going to predict the nightly rate for airbnb rentals in NYC. Let's first
# explore the data set.

# First let's confirm the data is tidy
airbnb_nyc %>% as_tibble()

# Let's explore the features and classes
glimpse(airbnb_nyc)

# We see there are 38,277 rows and 18 columns.

# We see some data points are showing na values. Let's address those.

# First, if there are na reviews_per_month, that can be extrapolated to mean
# there are zero reviews per month for that locale.
airbnb_nyc$reviews_per_month[is.na(airbnb_nyc$reviews_per_month)] <- 0

# We also see some na values for the names of some listings. We can safely assume
# that these names could be empty strings.
airbnb_nyc$name[is.na(airbnb_nyc$name)] <- ""

# Next, we see that the license field is na for all the rows so we should drop 
# that column
airbnb_nyc <- subset(airbnb_nyc, select = -c(license, name, host_name))

# We can also see that some listings have not yet been reviewed. Since we should
# add some value here, we can set the value to Jan 1, 1900 if it has not been 
# reviewed since this will be a tell for us.
airbnb_nyc$last_review[is.na(airbnb_nyc$last_review)] <- as.Date('1900-01-01', format="%Y-%m-%d")

# Also, since the last reviewed date isn't too useful for us in its current format,
# let's convert the value to be the number of days since the listing's last review.
airbnb_nyc <- airbnb_nyc %>% 
  mutate(last_review = as.numeric(
    difftime(as.Date('2022-03-31', format="%Y-%m-%d"), airbnb_nyc$last_review, units = "days")
  ))

names(airbnb_nyc)[names(airbnb_nyc) == "last_review"] <- "days_since_review"
head(airbnb_nyc)

# Looking at the neighbourhood_group column, we see that this represents the 
# different boroughs in New York City. Let's rename the column to borough to make 
# the name more accurate.

names(airbnb_nyc)[names(airbnb_nyc) == "neighbourhood_group"] <- "borough"
head(airbnb_nyc)

# Although this is minor, we should also convert the name of the neighbourhood
# to have the Americanized spelling of neighborhood

names(airbnb_nyc)[names(airbnb_nyc) == "neighbourhood"] <- "neighborhood"
head(airbnb_nyc)

# We also see that the neighborhood, borough, and room_type fields
# are shown as "character" columns when they should be factors. Let's convert the 
# types to be factors.

airbnb_nyc$neighborhood <- as.factor(airbnb_nyc$neighborhood)
airbnb_nyc$borough <- as.factor(airbnb_nyc$borough)
airbnb_nyc$room_type <- as.factor(airbnb_nyc$room_type)

# Let's explore the features and classes once more
glimpse(airbnb_nyc)

# That looks better!

# Now let's look at the total number of unique listings, hosts, neighborhoods, 
# boroughs, and room_types
airbnb_nyc %>% summarize(unique_listings = length(unique(id)),
                  unique_hosts = length(unique(host_id)),
                  unique_neighborhoods = length(unique(neighborhood)),
                  unique_boroughs = length(unique(borough)),
                  unique_room_types = length(unique(room_type)))
# We see 38,277 unique listings, 25,904 hosts, 222 neighborhoods, 
# 5 boroughs, and 4 room types.

# Let's now look at the most expensive airbnbs.
airbnb_nyc %>% arrange(-price)
# The most expensive airbnbs are $10,000 per night. That seems absurd by normal
# standards but it's totally possible in New York.

# Now let's look at the least expensive airbnbs.
airbnb_nyc %>% arrange(price)
# Hmm.. there are some airbnbs that charge $0 per night. That doesn't look right!
# Considering these data errors, let's remove these listings.

airbnb_nyc <- airbnb_nyc %>%
  filter(price > 0) %>% 
  arrange(price)

# Now let's look at the least expensive airbnbs again. 
airbnb_nyc %>% arrange(price)

# That looks better!

# Now let's see the distribution of listings across neighborhoods.
airbnb_nyc %>% 
  group_by(neighborhood) %>% 
  summarize(n_listings = n()) %>% 
  ggplot(aes(n_listings))+
  geom_histogram(fill = "slategray3", color = "black", bins = 35) +
  scale_x_log10()+
  ggtitle("Neighborhood Distribution")+
  xlab("Listings")+
  ylab("Neighborhoods")+
  theme(plot.title = element_text(hjust = 0.5))
# We can see an approximately normal distribution!

# Now let's see the distribution of listings across availability from the past 
# 365 days.
airbnb_nyc %>% 
  group_by(availability_365) %>% 
  summarize(n_listings = n()) %>% 
  ggplot(aes(n_listings))+
  geom_histogram(fill = "slategray3", color = "black", bins = 35) +
  scale_x_log10()+
  ggtitle("Availability Distribution")+
  xlab("Listings")+
  ylab("Availability")+
  theme(plot.title = element_text(hjust = 0.5))
# We see that the distribution of availabilities is also approximately normal.

# Let's look at the average nightly rate for each borough. We can 
# see that Manhattan is the most expensive and the Bronx is the least expensive.
# This makes sense based on general knowledge of New York boroughs.

airbnb_nyc %>%
  group_by(borough) %>%
  summarize(avg_nightly_rate = mean(price)) %>%
  ggplot(aes(borough, avg_nightly_rate)) + 
  geom_bar(stat="identity", fill = "slategray3") + 
  ggtitle("Average Nightly Rates by Borough") +
  xlab("Borough") +
  ylab("Average Nightly Rate") + 
  theme(plot.title = element_text(hjust = 0.5))

# Now let's look at the average nightly rate for each neighborhood. 
airbnb_nyc %>%
  group_by(neighborhood) %>%
  summarize(avg_nightly_rate = mean(price), listing_count = n()) %>%
  arrange(-avg_nightly_rate)

# We see that Jamaica Estates is listed as the most expensive neighborhood in New
# York. Knowing what we know about New York City neighborhoods, we know that this 
# cannot be true meaning that we have outliers. Let's go ahead and only include 
# listings that are within the 25th and 75th percentiles to cut out the outliers.

# We can presume that room type and borough will be some of the most
# influential factors in price determination. Let's go ahead and remove the
# outliers both at the room type and borough levels.

# Let's make a helper function that will do the filtering for us and give us back
# our outliers.

find_outliers <- function(listing_group) {
  lower_percentile <- 0.25
  upper_percentile <- 0.75
  lower_bound <- quantile(listing_group$price, lower_percentile)
  upper_bound <- quantile(listing_group$price, upper_percentile)
  outliers <- listing_group %>%
    filter(price > upper_bound | price < lower_bound)
  outliers
}

shared_rooms <- airbnb_nyc %>%
  filter(room_type == "Shared room")

###################################

shared_bronx_rooms <- shared_rooms %>%
  filter(borough == "Bronx")

# Let's first check the dimensions to make sure this is a valid subcategory.
dim(shared_bronx_rooms)

outliers <- find_outliers(shared_bronx_rooms)
airbnb_nyc <- anti_join(airbnb_nyc, outliers)

###################################

shared_brooklyn_rooms <- shared_rooms %>%
  filter(borough == "Brooklyn")

dim(shared_brooklyn_rooms)

outliers <- find_outliers(shared_brooklyn_rooms)
airbnb_nyc <- anti_join(airbnb_nyc, outliers)

###################################

shared_manhattan_rooms <- shared_rooms %>%
  filter(borough == "Manhattan")

dim(shared_manhattan_rooms)

outliers <- find_outliers(shared_manhattan_rooms)
airbnb_nyc <- anti_join(airbnb_nyc, outliers)

###################################

shared_queens_rooms <- shared_rooms %>%
  filter(borough == "Queens")

dim(shared_queens_rooms)

outliers <- find_outliers(shared_queens_rooms)
airbnb_nyc <- anti_join(airbnb_nyc, outliers)

###################################

shared_si_rooms <- shared_rooms %>%
  filter(borough == "Staten Island")

dim(shared_si_rooms)

outliers <- find_outliers(shared_si_rooms)
airbnb_nyc <- anti_join(airbnb_nyc, outliers)

###################################
###################################
###################################

private_rooms <- airbnb_nyc %>%
  filter(room_type == "Private room")

###################################

private_bronx_rooms <- private_rooms %>%
  filter(borough == "Bronx")

dim(private_bronx_rooms)

outliers <- find_outliers(private_bronx_rooms)
airbnb_nyc <- anti_join(airbnb_nyc, outliers)

###################################

private_brooklyn_rooms <- private_rooms %>%
  filter(borough == "Brooklyn")

dim(private_brooklyn_rooms)

outliers <- find_outliers(private_brooklyn_rooms)
airbnb_nyc <- anti_join(airbnb_nyc, outliers)

###################################

private_manhattan_rooms <- private_rooms %>%
  filter(borough == "Manhattan")

dim(private_manhattan_rooms)

outliers <- find_outliers(private_manhattan_rooms)
airbnb_nyc <- anti_join(airbnb_nyc, outliers)

###################################

private_queens_rooms <- private_rooms %>%
  filter(borough == "Queens")

dim(private_queens_rooms)

outliers <- find_outliers(private_queens_rooms)
airbnb_nyc <- anti_join(airbnb_nyc, outliers)

###################################

private_si_rooms <- private_rooms %>%
  filter(borough == "Staten Island")

dim(private_si_rooms)

outliers <- find_outliers(private_si_rooms)
airbnb_nyc <- anti_join(airbnb_nyc, outliers)

###################################
###################################
###################################

entire_place <- airbnb_nyc %>%
  filter(room_type == "Entire home/apt")

###################################

entire_bronx_place <- entire_place %>%
  filter(borough == "Bronx")

dim(entire_bronx_place)

outliers <- find_outliers(entire_bronx_place)
airbnb_nyc <- anti_join(airbnb_nyc, outliers)

###################################

entire_brooklyn_place <- entire_place %>%
  filter(borough == "Brooklyn")

dim(entire_brooklyn_place)

outliers <- find_outliers(entire_brooklyn_place)
airbnb_nyc <- anti_join(airbnb_nyc, outliers)

###################################

entire_manhattan_place <- entire_place %>%
  filter(borough == "Manhattan")

dim(entire_manhattan_place)

outliers <- find_outliers(entire_manhattan_place)
airbnb_nyc <- anti_join(airbnb_nyc, outliers)

###################################

entire_queens_place <- entire_place %>%
  filter(borough == "Queens")

dim(entire_queens_place)

outliers <- find_outliers(entire_queens_place)
airbnb_nyc <- anti_join(airbnb_nyc, outliers)

###################################

entire_si_place <- entire_place %>%
  filter(borough == "Staten Island")

dim(entire_si_place)

outliers <- find_outliers(entire_si_place)
airbnb_nyc <- anti_join(airbnb_nyc, outliers)

###################################
###################################
###################################

hotel_room <- airbnb_nyc %>%
  filter(room_type == "Hotel room")

###################################

bronx_hotel <- hotel_room %>%
  filter(borough == "Bronx")

dim(bronx_hotel)

# There doesn't look to be any hotels in the Bronx

###################################

brooklyn_hotel <- hotel_room %>%
  filter(borough == "Brooklyn")

dim(brooklyn_hotel)

outliers <- find_outliers(brooklyn_hotel)
airbnb_nyc <- anti_join(airbnb_nyc, outliers)

###################################

manhattan_hotel <- hotel_room %>%
  filter(borough == "Manhattan")

dim(manhattan_hotel)

outliers <- find_outliers(manhattan_hotel)
airbnb_nyc <- anti_join(airbnb_nyc, outliers)

###################################

queens_hotel <- hotel_room %>%
  filter(borough == "Queens")

dim(queens_hotel)

outliers <- find_outliers(queens_hotel)
airbnb_nyc <- anti_join(airbnb_nyc, outliers)

###################################

si_hotel <- hotel_room %>%
  filter(borough == "Staten Island")

dim(si_hotel)

# There doesn't seem to be any hotels in Staten Island either.

###################################
###################################
###################################

# Let's now separate our training set from our validation set.
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = airbnb_nyc$price, times = 1, p = 0.1, list = FALSE)
airbnb_train <- airbnb_nyc[-test_index,]
validation <- airbnb_nyc[test_index,]

# Let's now separate our training set into a train_set and test_set
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = airbnb_train$price, times = 1, p = 0.1, list = FALSE)
train_set <- airbnb_train[-test_index,]
test_set <- airbnb_train[test_index,]

test_set <- test_set %>% 
  semi_join(train_set, by = "neighborhood") %>%
  semi_join(train_set, by = "room_type")%>%
  semi_join(train_set, by = "availability_365")

validation <- validation %>% 
  semi_join(train_set, by = "neighborhood") %>%
  semi_join(train_set, by = "room_type") %>%
  semi_join(train_set, by = "availability_365")

# Let's now start our modeling.

# Let's try using RMSE

# This is our RMSE function that we will be using throughout.
RMSE <- function(true_price, predicted_price){
  sqrt(mean((true_price - predicted_price)^2))
}

# First we build the simplest system - recommending the average price
# for every prediction.
mu_hat <- mean(train_set$price)
naive_rmse <- RMSE(test_set$price, mu_hat)
# Let's visualize the results in a table
results <- tibble(Model_Type = "Mean Model", RMSE = naive_rmse) %>%
  mutate(RMSE = sprintf("%0.4f", RMSE))
results
# We see our RMSE at to 60.1709 - not a great start! Let's see if we can get 
# that lower.

mu <- mean(train_set$price)
bi_avgs <- train_set %>%
  group_by(neighborhood) %>%
  summarize(b_i = mean(price - mu))

bi_predictions <- mu + test_set %>%
  left_join(bi_avgs, by = "neighborhood") %>%
  pull(b_i)
bi_rmse <- RMSE(test_set$price, bi_predictions)
# Let's visualize the results in a table
results <- tibble(Model_Type = c(
  "Mean Model",
  "Neighborhood Model"
), RMSE = c(
  naive_rmse,
  bi_rmse
)) %>%
  mutate(RMSE = sprintf("%0.4f", RMSE))
results
# We see our RMSE drop to 52.2709!

# Keeping this in mind, let's investigate the RMSE with both the neighborhood 
# and availability 365 effects.
bu_avgs <- train_set %>%
  left_join(bi_avgs, by = "neighborhood") %>%
  group_by(availability_365) %>%
  summarize(b_u = mean(price - mu - b_i))
bu_predictions <- test_set %>%
  left_join(bi_avgs, by = "neighborhood") %>%
  left_join(bu_avgs, by = "availability_365")  %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
bu_rmse <- RMSE(test_set$price, bu_predictions)

# Let's once again visualize the results in a table
results <- tibble(Model_Type = c(
  "Mean Model",
  "Neighborhood Model",
  "Neighborhood & Availability 365 Model"
), RMSE = c(
  naive_rmse,
  bi_rmse,
  bu_rmse
)) %>%
  mutate(RMSE = sprintf("%0.4f", RMSE))
results

# We see a slight improvement to 52.8127!

# Keeping this in mind, let's investigate the RMSE with both the neighborhood, 
# availability 365, room type effects.
bv_avgs <- train_set %>%
  left_join(bi_avgs, by = "neighborhood") %>%
  left_join(bu_avgs, by = "availability_365") %>%
  group_by(room_type) %>%
  summarize(b_v = mean(price - mu - b_i - b_u))
bv_predictions <- test_set %>%
  left_join(bi_avgs, by = "neighborhood") %>%
  left_join(bu_avgs, by = "availability_365") %>%
  left_join(bv_avgs, by = "room_type") %>%
  mutate(pred = mu + b_i + b_u + b_v) %>%
  pull(pred)
bv_rmse <- RMSE(test_set$price, bv_predictions)

# Let's once again visualize the results in a table
results <- tibble(Model_Type = c(
  "Mean Model",
  "Neighborhood Model",
  "Neighborhood & Availability 365 Model",
  "Neighborhood, Availability 365, & Room Type Model"
), RMSE = c(
  naive_rmse,
  bi_rmse,
  bu_rmse,
  bv_rmse
)) %>%
  mutate(RMSE = sprintf("%0.4f", RMSE))
results

# We see the biggest improvement when we incorporate the room type effects into
# our model. Our RMSE came all the way down to 34.2903.

# REGULARIZATION

lambdas <- seq(0, 100, 1)
RMSES <- sapply(lambdas, function(l){
  train_mean <- mean(train_set$price)
  
  bi <- train_set %>%
    group_by(neighborhood) %>%
    summarize(bi = sum(price - train_mean)/(n() + l))
  
  bu <- train_set %>%
    left_join(bi, by='neighborhood') %>% 
    group_by(availability_365) %>%
    summarize(bu = sum(price - bi - train_mean)/(n() + l))
  
  predictions <- test_set %>%
    left_join(bi, by = "neighborhood") %>%
    left_join(bu, by = "availability_365") %>%
    mutate(pred = train_mean + bi +  bu) %>% 
    .$pred
  
  return(RMSE(predictions, test_set$price))
})

# Let's find the lambda that minimizes RMSE
na_model_lambda <- lambdas[which.min(RMSES)]
na_model_lambda

# A lambda of 13 minimizes our RMSE

# Let's now re-examine our neighborhood and availability effects model with
# regularization
b_i <- train_set %>%
  group_by(neighborhood) %>%
  summarize(b_i = sum(price - mu)/(n() + na_model_lambda))
b_u <- train_set %>%
  left_join(b_i, by = "neighborhood") %>%
  group_by(availability_365) %>%
  summarize(b_u = sum(price - b_i - mu)/(n() + na_model_lambda))
bu_predictions <- test_set %>%
  left_join(b_i, by = "neighborhood") %>%
  left_join(b_u, by = "availability_365") %>%
  mutate(predictions = mu + b_i + b_u) %>%
  .$predictions

bu_reg_rmse <- RMSE(test_set$price, bu_predictions)

# Let's chart our results.

results <- tibble(Model_Type = c(
  "Mean Model",
  "Neighborhood Model",
  "Neighborhood & Availability 365 Model",
  "Neighborhood, Availability 365, & Room Type Model",
  "Neighborhood, Availability 365 Model w/ Reg"
), RMSE = c(
  naive_rmse,
  bi_rmse,
  bu_rmse,
  bv_rmse,
  bu_reg_rmse
)) %>%
  mutate(RMSE = sprintf("%0.4f", RMSE))
results

# We see that regularization slightly improved our RMSE to 52.1545 for the same 
# model without regularization.

# Let's see how regularization helps our neighborhood, availability from the last 
# 365 days, and room type effects model.

# Once again, we will find the lambda that minimizes our RMSE.

lambdas <- seq(0, 100, 1)
RMSES <- sapply(lambdas, function(l){
  train_mean <- mean(train_set$price)
  
  bi <- train_set %>%
    group_by(neighborhood) %>%
    summarize(bi = sum(price - train_mean)/(n() + l))
  
  bu <- train_set %>%
    left_join(bi, by='neighborhood') %>% 
    group_by(availability_365) %>%
    summarize(bu = sum(price - bi - train_mean)/(n() + l))
  
  br <- train_set %>%
    left_join(bi, by = "neighborhood") %>%
    left_join(bu, by = "availability_365") %>%
    group_by(room_type) %>%
    summarize(br = sum(price - bi - bu - train_mean)/(n() + l))
  
  predictions <- test_set %>%
    left_join(bi, by = "neighborhood") %>%
    left_join(bu, by = "availability_365") %>%
    left_join(br, by = "room_type") %>%
    mutate(pred = train_mean + bi +  bu + br) %>% 
    .$pred
  
  return(RMSE(predictions, test_set$price))
})

#Let's find the lambda that minimizes RMSE
nar_model_lambda <- lambdas[which.min(RMSES)]
nar_model_lambda

# We see a lambda of 70 minimizes our RMSE.

# Let's re-train our neighborhood, availability from the past 365 days, and room
# type model with regularization.
b_i <- train_set %>%
  group_by(neighborhood) %>%
  summarize(b_i = sum(price - mu)/(n() + nar_model_lambda))
b_u <- train_set %>%
  left_join(b_i, by = "neighborhood") %>%
  group_by(availability_365) %>%
  summarize(b_u = sum(price - b_i - mu)/(n() + nar_model_lambda))
b_r <- train_set %>%
  left_join(b_i, by = "neighborhood") %>%
  left_join(b_u, by = "availability_365") %>%
  group_by(room_type) %>%
  summarize(b_r = sum(price - b_i - b_u - mu)/(n() + nar_model_lambda))

bv_predictions <- test_set %>%
  left_join(b_i, by = "neighborhood") %>%
  left_join(b_u, by = "availability_365") %>%
  left_join(b_r, by = "room_type") %>%
  mutate(predictions = mu + b_i + b_u + b_r) %>%
  .$predictions

bv_reg_rmse <- RMSE(test_set$price, bv_predictions)

# Let's visualize the results once more.
results <- tibble(Model_Type = c(
  "Mean Model",
  "Neighborhood Model",
  "Neighborhood & Availability 365 Model",
  "Neighborhood, Availability 365, & Room Type Model",
  "Neighborhood, Availability 365 Model w/ Reg",
  "Neighborhood, Availability 365, & Room Type w/ Reg"
), RMSE = c(
  naive_rmse,
  bi_rmse,
  bu_rmse,
  bv_rmse,
  bu_reg_rmse,
  bv_reg_rmse
)) %>%
  mutate(RMSE = sprintf("%0.4f", RMSE))
results

# We see a significant improvement to 30.9987, around a 10% improvement.

# Let's now see if we can do even better with matrix factorization.
set.seed(1, sample.kind="Rounding")
train_reco <- with(train_set, data_memory(user_index = neighborhood, item_index = room_type, rating = price))
test_reco <- with(test_set, data_memory(user_index = neighborhood, item_index = room_type, rating = price))
r <- Reco()

para_reco <- r$tune(train_reco, opts = list(dim = c(20, 30),
                                            costp_l2 = c(0.01, 0.1),
                                            costq_l2 = c(0.01, 0.1),
                                            lrate = c(0.01, 0.1),
                                            nthread = 4,
                                            niter = 10))

r$train(train_reco, opts = c(para_reco$min, nthread = 4, niter = 30))
results_reco <- r$predict(test_reco, out_memory())

factorization_rmse <- RMSE(results_reco, test_set$price)

# Let's visualize the results.

results <- tibble(Model_Type = c(
  "Mean Model",
  "Neighborhood Model",
  "Neighborhood & Availability 365 Model",
  "Neighborhood, Availability 365, & Room Type Model",
  "Neighborhood, Availability 365 Model w/ Reg",
  "Neighborhood, Availability 365, & Room Type w/ Reg",
  "Matrix Factorization"
), RMSE = c(
  naive_rmse,
  bi_rmse,
  bu_rmse,
  bv_rmse,
  bu_reg_rmse,
  bv_reg_rmse,
  factorization_rmse
)) %>%
  mutate(RMSE = sprintf("%0.4f", RMSE))
results

# We got our RMSE down to 28.59776! Considering the range of prices available, this
# is a great RMSE value.

# Let's now see how our model does against our validation set.

# VALIDATION DATA

# As a benchmark, let's first see the performance of the mean model on the validation
# set.
train_mean <- mean(train_set$price)
validation_mean_model <- RMSE(validation$price, train_mean)

results <- tibble(Model_Type = ("Mean Model"),
                  Validation_RMSE = (validation_mean_model)) %>%
  mutate(Validation_RMSE = sprintf("%0.5f", Validation_RMSE))
results

# We see the RMSE at 61.40009 - not too far off from the RMSE on the test set.

# Let's now see how our neighborhood effects model does.
bi <- train_set %>% 
  group_by(neighborhood) %>%
  summarize(b_i = mean(price - train_mean))
bi_prediction <- train_mean + validation %>%
  left_join(bi, by = "neighborhood") %>%
  .$b_i
bi_rmse_validation <- RMSE(validation$price, bi_prediction)
results <- tibble(Model_Type = c(
  "Mean Model",
  "Neighborhood Model"
),
Validation_RMSE = c(
  validation_mean_model,
  bi_rmse_validation
)) %>%
  mutate(Validation_RMSE = sprintf("%0.5f", Validation_RMSE))
results
# We see an RMSE of 52.68041 - slightly worse than how our model did on the test set.

# Now let's see how our neighborhood and availability effects model does on the 
# validation set.
bi <- train_set %>% 
  group_by(neighborhood) %>%
  summarize(b_i = mean(price - train_mean))
bu <- train_set %>% 
  left_join(bi, by = "neighborhood") %>%
  group_by(availability_365) %>%
  summarize(b_u = mean(price - train_mean - b_i))
bu_prediction <- validation %>%
  left_join(bi, by = "neighborhood") %>%
  left_join(bu, by = "availability_365") %>%
  mutate(predictions = train_mean + b_i + b_u) %>%
  .$predictions
bu_rmse_validation <- RMSE(validation$price, bu_prediction)
results <- tibble(Model_Type = c(
  "Mean Model",
  "Neighborhood Model",
  "Neighborhood & Availability 365 Model"
),
Validation_RMSE = c(
  validation_mean_model,
  bi_rmse_validation,
  bu_rmse_validation
)) %>%
  mutate(Validation_RMSE = sprintf("%0.5f", Validation_RMSE))
results
# We achieved an RMSE of 52.53051 - slightly better than the performance on the 
# test set.

# Now let's see how our neighborhood, availability, and room type effects model 
# performs.
bi <- train_set %>% 
  group_by(neighborhood) %>%
  summarize(b_i = mean(price - train_mean))
bu <- train_set %>% 
  left_join(bi, by = "neighborhood") %>%
  group_by(availability_365) %>%
  summarize(b_u = mean(price - train_mean - b_i))
br <- train_set %>%
  left_join(bi, by = "neighborhood") %>%
  left_join(bu, by = "availability_365") %>%
  group_by(room_type) %>%
  summarize(b_r = mean(price - train_mean - b_i - b_u))
bv_prediction <- validation %>%
  left_join(bi, by = "neighborhood") %>%
  left_join(bu, by = "availability_365") %>%
  left_join(br, by = "room_type") %>%
  mutate(predictions = train_mean + b_i + b_u + b_r) %>%
  .$predictions
bv_val_rmse <- RMSE(validation$price, bv_prediction)
results <- tibble(Model_Type = c(
  "Mean Model",
  "Neighborhood Model",
  "Neighborhood & Availability 365 Model",
  "Neighborhood, Availability 365, & Room Type Model"
),
Validation_RMSE = c(
  validation_mean_model,
  bi_rmse_validation,
  bu_rmse_validation,
  bv_val_rmse
)) %>%
  mutate(Validation_RMSE = sprintf("%0.5f", Validation_RMSE))
results
# We see an RMSE of 34.61761 - slightly worse than the test set RMSE.

# Now let's see how our neighborhood and availability model with regularization 
# does.
b_i <- train_set %>%
  group_by(neighborhood) %>%
  summarize(b_i = sum(price - mu)/(n() + na_model_lambda))
b_u <- train_set %>%
  left_join(b_i, by = "neighborhood") %>%
  group_by(availability_365) %>%
  summarize(b_u = sum(price - b_i - mu)/(n() + na_model_lambda))
bu_val_predictions <- validation %>%
  left_join(b_i, by = "neighborhood") %>%
  left_join(b_u, by = "availability_365") %>%
  mutate(predictions = mu + b_i + b_u) %>%
  .$predictions

bu_val_reg_rmse <- RMSE(validation$price, bu_val_predictions)

results <- tibble(Model_Type = c(
  "Mean Model",
  "Neighborhood Model",
  "Neighborhood & Availability 365 Model",
  "Neighborhood, Availability 365, & Room Type Model",
  "Neighborhood & Availability 365 Model w/ Reg"
),
Validation_RMSE = c(
  validation_mean_model,
  bi_rmse_validation,
  bu_rmse_validation,
  bv_val_rmse,
  bu_val_reg_rmse
)) %>%
  mutate(Validation_RMSE = sprintf("%0.5f", Validation_RMSE))
results
# We achieve an RMSE of 52.37774 - slight worse than our test set RMSE.

# Now let's see how our neighborhood, availability, and room type model with 
# regularization performs.
b_i <- train_set %>%
  group_by(neighborhood) %>%
  summarize(b_i = sum(price - mu)/(n() + nar_model_lambda))
b_u <- train_set %>%
  left_join(b_i, by = "neighborhood") %>%
  group_by(availability_365) %>%
  summarize(b_u = sum(price - b_i - mu)/(n() + nar_model_lambda))
b_v <- train_set %>%
  left_join(b_i, by = "neighborhood") %>%
  left_join(b_u, by = "availability_365") %>%
  group_by(room_type) %>%
  summarize(b_v = sum(price - b_i - b_u - mu)/(n() + nar_model_lambda))

bv_val_predictions <- validation %>%
  left_join(b_i, by = "neighborhood") %>%
  left_join(b_u, by = "availability_365") %>%
  left_join(b_v, by = "room_type") %>%
  mutate(predictions = mu + b_i + b_u + b_v) %>%
  .$predictions

bv_val_reg_rmse <- RMSE(validation$price, bv_val_predictions)

results <- tibble(Model_Type = c(
  "Mean Model",
  "Neighborhood Model",
  "Neighborhood & Availability 365 Model",
  "Neighborhood, Availability 365, & Room Type Model",
  "Neighborhood, Availability 365 Model w/ Reg",
  "Neighborhood, Availability 365, & Room Type w/ Reg"
), Validation_RMSE = c(
  validation_mean_model,
  bi_rmse_validation,
  bu_rmse_validation,
  bv_val_rmse,
  bu_val_reg_rmse,
  bv_val_reg_rmse
)) %>%
  mutate(Validation_RMSE = sprintf("%0.4f", Validation_RMSE))
results

# We achieved an RMSE of 31.8404 - slightly worse than on the test set. 

# Lastly, let's see how our matrix factorization model does on our validation set.
val_reco <- with(validation, data_memory(
  user_index = neighborhood, 
  item_index = room_type, 
  rating = price))
results_reco <- r$predict(val_reco, out_memory())

factorization_val_rmse <- RMSE(results_reco, validation$price)

results <- tibble(Model_Type = c(
  "Mean Model",
  "Neighborhood Model",
  "Neighborhood & Availability 365 Model",
  "Neighborhood, Availability 365, & Room Type Model",
  "Neighborhood, Availability 365 Model w/ Reg",
  "Neighborhood, Availability 365, & Room Type w/ Reg",
  "Matrix Factorization"
), Validation_RMSE = c(
  validation_mean_model,
  bi_rmse_validation,
  bu_rmse_validation,
  bv_val_rmse,
  bu_val_reg_rmse,
  bv_val_reg_rmse,
  factorization_val_rmse
)) %>%
  mutate(Validation_RMSE = sprintf("%0.4f", Validation_RMSE))
results
# We were able to achieve an RMSE of 29.04977 - slightly worse than our performance 
# on the test set.

# Now let's visualize our RMSE values for both our test and validation sets.
results <- tibble(Model_Type = c(
  "Mean Model",
  "Neighborhood Model",
  "Neighborhood & Availability 365 Model",
  "Neighborhood, Availability 365, & Room Type Model",
  "Neighborhood, Availability 365 Model w/ Reg",
  "Neighborhood, Availability 365, & Room Type w/ Reg",
  "Matrix Factorization"
),
RMSE = c(
  naive_rmse,
  bi_rmse,
  bu_rmse,
  bv_rmse,
  bu_reg_rmse,
  bv_reg_rmse,
  factorization_rmse
),
Validation_RMSE = c(
  validation_mean_model,
  bi_rmse_validation,
  bu_rmse_validation,
  bv_val_rmse,
  bu_val_reg_rmse,
  bv_val_reg_rmse,
  factorization_val_rmse
)) %>%
  mutate(Validation_RMSE = sprintf("%0.5f", Validation_RMSE))
results

results %>% knitr::kable()