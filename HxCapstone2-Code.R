###########################################################
#
#  R code showing analysis work performed by Tim Bishop
#  as part of the Hx Data Science Capstone module 
#  second project on a choose your own dataset
#
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###




###########################################################
#
# prepare dataset, and initial setup
# download dataset if it is not already in working directory
#
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# load packages used, download and install if necessary

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(purrr)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(corrgram)
library(plotly)
library(xgboost)
library(rpart)
library(rpart.plot)
library(pROC)


if (!file.exists("parkinsons.data") ){
  #dl <- tempfile()
  download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/parkinsons/parkinsons.data", "parkinsons.data")
 
}

data_original <- read.csv("parkinsons.data",header = TRUE)



###########################################################
#
# Initial Data Validation 
#
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###



# using basic summary stats to assess data provided 
summary(data_original)
status_means <- data.frame( Group = "Full Data Set", Status_Mean= mean(data_original$status))




###########################################################
#
# Split Dataset Into Training, Model Validation and Final Testing Subsets
#
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# split all data into test and training sets
set.seed(2021, sample.kind = "Rounding")    # to reproduce same samples splits & numbers, assuming R version 3.6 or higher
test_index <- createDataPartition(data_original$status,             # dependent variabls to split on
                                  times = 1,     # how many partitions / splits to return
                                  p = 0.1,       # % of data in test
                                  list = FALSE)  # don't return a list

data_test <- data_original[test_index,]
data_train <- data_original[-test_index,]

# split all training set into model validation and true training sets
#set.seed(2021, sample.kind = "Rounding")    # to reproduce same samples splits & numbers, assuming R version 3.6 or higher
validation_index <- createDataPartition(data_train$status,  # dependent variabls to split on
                                  times = 1,     # how many partitions / splits to return
                                  p = 0.1,       # % of data in test
                                  list = FALSE)  # don't return a list

data_validation <- data_train[validation_index,]
data_train <- data_train[-validation_index,]


sum_train <- summary(data_train)
sum_validation <- summary(data_validation)
sum_test <- summary(data_test)

#knitr::kable(sum_train)

sum_train 
sum_validation 
sum_test 

status_means[2,1] <- "Train Set"
status_means[2,2] <-  mean(data_train$status)
status_means[3,1] <- "Model Val Set"
status_means[3,2] <-  mean(data_validation$status)
status_means[4,1] <- "Test Set"
status_means[4,2] <-  mean(data_test$status)

status_means





# x_test <- x[test_index,]
# y_test <- y[test_index]
# x_train <- x[-test_index,]
# y_train <- y[-test_index]




###########################################################
#
#  Data Exploration on Training Data 
#
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###



# # using basic summary stats to assess data provided 
# summary(data_original)
# 
# # Observations, ???





# # Let's check the correlations shall we?
# corrgram(data_original[-1], order = NULL, lower.panel = panel.shade, 
#          upper.panel = NULL, text.panel = panel.txt, #label.srt = 90,
#          main = "Correlation among variables")


# preferred 
# Calculate correlation matrix
cormat <- cor(data_train[-1]) # exclude first column which is a character value for name
# Melt the correlation matrix
melted.cormat <- melt(cormat)
# Remember plotly from the welcome notes?
plot_ly(
  x = melted.cormat$Var1, y = melted.cormat$Var2,
  z = melted.cormat$value, colorscale = "Reds", type = "heatmap"
)


ncol(data_train)

# view distribution of each variable, where each variable is in a separate column in data
data_train[,2:11] %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

data_train[,12:24] %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()+
  theme_bw()



data_train[c(2:12,18)] %>% 
  gather(-status, key = "var", value = "value") %>% 
  ggplot(aes(x = status, y = value, color = status)) +
  geom_point() +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

data_train[c(13:24)] %>% 
  gather(-status, key = "var", value = "value") %>% 
  ggplot(aes(x = status, y = value, color = status)) +
  geom_point() +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

data_train[c(2:12,18)] %>% 
  gather(-status, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = status, color = status)) +
  geom_point() +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

data_train[c(13:24)] %>% 
  gather(-status, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = status, color = status)) +
  geom_point() +
  facet_wrap(~ var, scales = "free") +
  theme_bw()



# for PCA, remove the character string name field, and the dependent variable status field
# check indexes to exclude are correct
names(data_train[,c(1,18)])
y_train <- as.matrix( data_train[,18])
x_train <- as.matrix(data_train[,-c(1,18)])
y_test <- as.matrix( data_test[,18])
x_test <- as.matrix(data_test[,-c(1,18)])
  
y_train <-  data_train[,18]
x_train <- data_train[,-c(1,18)]
y_test <-  data_test[,18]
x_test <- data_test[,-c(1,18)]

model_pca <- prcomp(x_train, center = TRUE, scale. = TRUE)
summary(model_pca)
model_pca$rotation
# produce elbow plot
#ggplot()






# # Applying k-Fold Cross Validation
# # install.packages('caret')
# library(caret)
# folds = createFolds(training_set$Exited, k = 10)
# cv = lapply(folds, function(x) {
#   training_fold = training_set[-x, ]
#   test_fold = training_set[x, ]
#   classifier = xgboost(data = as.matrix(training_fold[-11]), label = training_fold$Exited, nrounds = 10)
#   y_pred = predict(classifier, newdata = as.matrix(test_fold[-11]))
#   y_pred = (y_pred >= 0.5)
#   cm = table(test_fold[, 11], y_pred)
#   accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
#   return(accuracy)
# })
# accuracy = mean(as.numeric(cv))


# Fitting XGBoost to the Training set
# install.packages('xgboost')

fit_xgb = xgboost(data = x_train, label = y_train, nrounds = 100)

# Predicting the Test set results
pred_xgb = predict(fit_xgb, newdata = x_test)
pred_xgb = as.integer(pred_xgb >= 0.5)

# Making the Confusion Matrix
#cm = table(y_test, y_pred)
cm_xgb = confusionMatrix(factor(pred_xgb, levels = c("1","0"), labels = c("Parkinson", "Healthy")), 
                     factor(y_test, levels = c("1","0"), labels = c("Parkinson", "Healthy")))
cm_xgb



# Trying a simple classification tree
#
# as a test case, use complexity parameter cp=0.001, meaning RMSE must decrease by 0.001 in order to create tree split
# given size of dateset, limit bucket size to 100,000 before split is considered and limit depth to 10 levels
# NOTE: dropping fields for rating timestamp, title and genres list as these are either redundant or appear too complex
fit_tree <- rpart(status ~ . , data = data.frame(data_train[-1]), method = "class", 
                  control = rpart.control(minbucket = 5, cp = .0001, maxdepth = 10))
pred_tree <- predict(fit_tree, data.frame(data_test[-1]), type = 'class')
#pred_tree = as.integer(pred_tree >= 0.5)

cm_tree = confusionMatrix(factor(pred_tree, levels = c("1","0"), labels = c("Parkinson", "Healthy")), 
                     factor( y_test, levels = c("1","0"), labels = c("Parkinson", "Healthy")))

cm_tree

rpart.plot(fit_tree)
plot(pred_tree)


roc_tree <- roc(y_test, pred_tree)
plot(roc_tree, xlim = c(1,0))
roc_tree$auc











###########################################################
#
#  FEATURE CREATION SECTION
#
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# As a first step to improving the predictions provided,
# additional features will be extracted from the data provided.
#
# Note: module 6.2 from Hx ML course covered the movielens predictor in some degree


# add feature for number of genres listed for each movie
# the premise is that "cross over" movies are likely more popular
# Similarly, popular movies may draw more genres since they are considered more
# Lastly, multi-genres movies likely draw more search hits, get more views and reviews.
edx <- edx %>% mutate(genres_count = sapply(genres, function(c){length(str_split(c,"\\|", simplify = TRUE))}))


# extract year of movie release from data field "title" in dataset
#NOTE: this takes a few minutes to run.
edx <- edx %>% mutate(movie_year = as.numeric(  str_sub(  str_extract(title,"\\(\\d{4}\\)") , 2, 5)))


# extract year that rating was provided
edx <- edx %>% mutate(rating_date = as.POSIXct(timestamp, origin ='1970-01-01') )
edx <- edx %>% mutate(rating_year = year(rating_date))


# create feature approximating age of movie at time of rating
edx <- mutate(edx, age_at_rating = rating_year - movie_year)


# data checks on new Features
min(edx$age_at_rating)
max(edx$age_at_rating)
table(edx$age_at_rating)

sum(edx$age_at_rating < 0)
# a small number of movies have rating coded in years prior to the movie release date in the title
# ASSUMING this is a data error and will set age at rating to 0 (ie rating is same year of release) for these
edx$age_at_rating[edx$age_at_rating < 0] <- 0

# take another look after removing negative ages at rating date
table(edx$age_at_rating)
summary(edx$age_at_rating)

# taking a quick look at the movie titles for the oldest movies.
edx$title[which(edx$movie_year < 1930)] # title look right to movie year

# look at reasonability of the new genres_count feature
table(edx$genres_count)



# data description as provided 
# from https://grouplens.org/datasets/movielens/latest/

# Genres are a pipe-separated list, and are selected from the following:
# 
# Action
# Adventure
# Animation
# Children
# Comedy
# Crime
# Documentary
# Drama
# Fantasy
# Film-Noir
# Horror
# Musical
# Mystery
# Romance
# Sci-Fi
# Thriller
# War
# Western
# (no genres listed)

# using the individual genres in the genres list and set up binary indicator variables for each 
# individual genres
# Note: GroupLens website (the source of the original data) was used to obtain the list of 
# possible individual genres.


edx <- mutate( edx,
               Action = as.integer( str_detect(genres, "Action")),
               Adventure = as.integer( str_detect(genres, "Adventure")),
               Animation = as.integer( str_detect(genres, "Animation")),
               Children = as.integer( str_detect(genres, "Children")),
               Comedy = as.integer( str_detect(genres, "Comedy")),
               Crime = as.integer( str_detect(genres, "Crime")),
               Documentary = as.integer( str_detect(genres, "Documentary")),
               Drama = as.integer( str_detect(genres, "Drama")),
               Fantasy = as.integer( str_detect(genres, "Fantasy")),
               FilmNoir = as.integer( str_detect(genres, "Film-Noir")),
               Horror = as.integer( str_detect(genres, "Horror")),
               Musical = as.integer( str_detect(genres, "Musical")),
               Mystery = as.integer( str_detect(genres, "Mystery")),
               Romance = as.integer( str_detect(genres, "Romance")),
               SciFi = as.integer( str_detect(genres, "Sci-Fi")),
               Thriller = as.integer( str_detect(genres, "Thriller")),
               War = as.integer( str_detect(genres, "War")),
               Western = as.integer( str_detect(genres, "Western"))
)
# note coding above implicitly creates a last category which is none of the above.
# explicitly encoding an "other" category is not required as the information is captured as zeros in all fields above


# use summary to scan for odd or missing values in new features
summary(edx)


# Investigating the whole star rating bias noted above.  Look for earliest years where whole or half star ratings appear
min(edx[ edx$rating %in% c(1,2,3,4,5),rating_year])
min(edx[ edx$rating %in% c(1.5,2.5,3.5,4.5),rating_year])
# clearly there is a change in ratings scales available to users prior to 2003 as 2002 and prior only have whole star ratings

# create new feature to indicate if rating is from year before half star ratings were allowed.
edx$pre_half_star <- ifelse(edx$rating_year < 2003,1,0)


# Trying some visualizations to see if some of the new features are related to variances in ratings


# visualize ratings over age of movie at time of rating
edx %>% ggplot( aes(age_at_rating, fill= as.factor(rating))) + 
  geom_bar(position = "stack") + 
  ggtitle("Counts of Rating At Movie Ages Split by Rating")

# visualize distribution of ratings at each age of movie at time of rating
edx %>% ggplot( aes(age_at_rating, fill= as.factor(rating))) + 
  geom_bar(position = "fill") + 
  ggtitle("Percentages of Each Rating by Age of Movie At Rating")


# visualize distribution of ratings over # of genres listed for a movie 
  edx %>% ggplot( aes(genres_count, fill= as.factor(rating))) + 
  geom_bar(position = "fill") + 
  ggtitle("Percentages of Each Rating by # of Genres Listed for a Movie")


  
  # some viualization to look at impact of genres on rating
  # Note: I found these visualizations to be hard to use so these
  # were not included in the report
  edx %>% ggplot( aes(Action, fill= as.factor(rating))) + 
    geom_bar(position = "fill") + 
    ggtitle("Percentages of Each Rating by Action VS Not")
  
  edx %>% ggplot( aes(Adventure, fill= as.factor(rating))) + 
    geom_bar(position = "fill") + 
    ggtitle("Percentages of Each Rating by Adventure VS Not")
  
  edx %>% ggplot( aes(Animation, fill= as.factor(rating))) + 
    geom_bar(position = "fill") + 
    ggtitle("Percentages of Each Rating by Animation VS Not")
  
  edx %>% ggplot( aes(Children, fill= as.factor(rating))) + 
    geom_bar(position = "fill") + 
    ggtitle("Percentages of Each Rating by Children VS Not")
  
  edx %>% ggplot( aes(Adventure, fill= as.factor(rating))) + 
    geom_bar(position = "fill") + 
    ggtitle("Percentages of Each Rating by Adventure VS Not")
  
  edx %>% ggplot( aes(Comedy, fill= as.factor(rating))) + 
    geom_bar(position = "fill") + 
    ggtitle("Percentages of Each Rating by Comedy VS Not")
  
  edx %>% ggplot( aes(Crime, fill= as.factor(rating))) + 
    geom_bar(position = "fill") + 
    ggtitle("Percentages of Each Rating by Crime VS Not")
  
  edx %>% ggplot( aes(Documentary, fill= as.factor(rating))) + 
    geom_bar(position = "fill") + 
    ggtitle("Percentages of Each Rating by Documentary VS Not")
  
  edx %>% ggplot( aes(Drama, fill= as.factor(rating))) + 
    geom_bar(position = "fill") + 
    ggtitle("Percentages of Each Rating by Drama VS Not")
  
  
  edx %>% ggplot( aes(Romance, fill= as.factor(rating))) + 
    geom_bar(position = "fill") + 
    ggtitle("Percentages of Each Rating by Romance VS Not")
  
  
  
##  AS an alternative, clearer method to see variations in rating by individual genres
#  some basic numeric data was set up to show variations in a tabular way. 
#  This was much easier to interpret and showed impact more clearly.
  
# find average rating
overall_ave_rating <- mean(edx$rating)

# calculate average rating for each count of individual genres listed for a movie
ave_ratings_by_genres_count <- edx %>% group_by(genres_count) %>%
      summarise(ave_rating = sum(rating)/n())

# show table of average rating by a movie's genres count                               
ave_ratings_by_genres_count


# Build table of stats to assess impact of individual genres on a movie rating.
# calculate average rating for those movies tagged under each genre
ave_ratings_by_genres <- apply(edx[,Action:Western]*edx[,rating], 2, 
                               FUN = function(x){
                                 mean(x[x>0])
                               })
# show table
ave_ratings_by_genres


# create vector of differences between genres average rating and overall average.
# this more clearly shows which individual genres have large differences from the overall average
ave_rating_diff_by_genres <- round(ave_ratings_by_genres - overall_ave_rating, digits = 2)


# this section is really only a check that the percent_by_genres is not a true distribution and don't sum to 100%
# since a given movie can be listed under several genres, so there is overlap.
#
# for a given genre, the percent_by_genres actually shows the % of all movies listed in that genre, even if listed in other
# genres as well.
percent_by_genres <- apply(edx[,Action:Western], 2, FUN = sum)
percent_by_genres <- percent_by_genres / nrow(edx) * 100
percent_by_genres
sum(percent_by_genres)


# creating my own rating "impact_score" 
# the total RMSE impact for a genres indicator will depend on the # of movies listed under that genres 
# AND how much that genre's average score is different from the overall average.
# This is an ad hoc, indicative score, not a mathematically rigorous measure.
# It is simple, easy to understand and it works though.
impact_score <- abs( round( percent_by_genres * ave_rating_diff_by_genres, 1))
impact_table <- t(rbind(ave_ratings_by_genres,percent_by_genres, ave_rating_diff_by_genres,
                        impact_score))


# Now that we have a table showing several indicators of how each individual genre explains rating variances
# Lets look at this table sorted by some of these different measures.
# By sorting and re-showing, it makes review and interpretation easier so we can tease out what genres to 
# includ in the model

#sort by impact
impact_table <- impact_table[ order(impact_table[,4], decreasing = TRUE),]
#show impact table
knitr::kable(impact_table)

#sort by difference from total average in absolute amounts
impact_table <- impact_table[ order(abs(impact_table[,3]), decreasing = TRUE),]
#show impact table
knitr::kable(impact_table)

#sort by percentage of movies in that genre
impact_table <- impact_table[ order(abs(impact_table[,2]), decreasing = TRUE),]
#show impact table
knitr::kable(impact_table)






###########################################################
#
#  To start the modeling work, 
#  SPLIT "edx" DATASET INTO A TRAINING SET AND A SECOND
#  INDEPENDENT SET TO USE FOR TESTING MODELS FOR THE PURPOSE
#  OF COMPARING AND/OR SELECTING THE BEST MODEL.
#  THE "validation" DATASET PROVIDED WILL ONLY BE USED FOR
#  SHOWING THE PERFORMANCE OF THE FINAL MODEL SELECTED.
#
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

set.seed(1999) # for reproduceability of results

# The training dataset "edx" will be split into a true training set and a second independent 
# set for validating or testing models for the purpose of comparing model performance between 
# models being considered.  The final model chosen will be ultimately tested on the test 
# dataset provided, which is named "validation"

modelval_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
train_set <- edx[-modelval_index,]
temp <- edx[modelval_index,]

# needed for later join functions to always match an not introduce NA's
modelval_set <- temp %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")


# Add rows removed from validation set back into edx set
removed <- anti_join(temp, modelval_set)
train_set <- rbind(train_set, removed)

rm(modelval_index, temp, removed)

# do quick check that the two datasets look similar and we didn't introduce sampling error when splitting
summary(train_set)
summary(modelval_set)





###########################################################
#
#  BASELINE MODEL 
#  RECREATE TEXTBOOK EXAMPLE ON MOVIELENS
#
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

#  Since the objective is to "beat the RMSE of the prior Hx course"
#  Let's start by redoing what was done before for a comparison, starting point.
#
# Reproduce results from Hx Machine Learning course
# This is done as a baseline model from which to extend 
# or replace with a better model
#
#
# Normalized Model with 3 effects

# lambda values to be used for cross validation to find best lambda for regularization
# optimal lambda could be between this grid of points, but I did not pursue this as
# results appeared to be pretty good without spending a lot more time on computation
# Initial lambda grid is wide.  Later I narror the range tested as best lambda values
# were always in the same close range of 4.75 to 5.
lambdas <- seq(0, 8, 0.25)

# fit model over each lambda value to tune model for lambda regularization parameter
rmses <- sapply(lambdas, function(l){
  
  print( paste("fitting lambda =", as.character(l))) # show progress during execution 
  
  # Fit user affect and movie affect model on train set
  mu <- mean(train_set$rating)
  
  # movie effect
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  # user effect
  b_u <- train_set %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  # using fit model, create predictions on model validation test set
  predicted_rating <-    modelval_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_rating, modelval_set$rating))
})

# plot results by each lambda value
qplot(lambdas, rmses)

# pick best lambda and associated result
best_lambda <- lambdas[which.min(rmses)]
print(paste("Best lambda = ", as.character(best_lambda)))
model_base_rmse <- min(rmses)

print("Base Model from textbook:  Liner model using two predictors")
print("rating ~ movieId + userId")
print(paste("Provides RMSE = " , as.character(model_base_rmse)))
#  returns RMSE = 0.864768869160857






###########################################################
#
#  EXTENDING BASELINE MODEL TO USE NEW FEATURES 
#
# adding age at review
#
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###



# Note: for run time, limiting range of lambda's tested since pattern of RMSE's is well behaved parabola shape
lambdas <- seq(4.5, 5.25, 0.25)


rmses <- sapply(lambdas, function(l){
  print( paste("fitting lambda =", as.character(l)))
  
  mu <- mean(train_set$rating)
  
  # movie effect
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  # user effect
  b_u <- train_set %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  # age at rating effect
  b_a <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by= "userId") %>%
    group_by(age_at_rating) %>%
    summarize(b_a = sum(rating - b_u - b_i - mu)/(n()+l))
  
  predicted_rating <-    modelval_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_a, by = "age_at_rating") %>%
    mutate(pred = mu + b_i + b_u + b_a) %>%
    pull(pred)
  
  model_rmse <- RMSE(predicted_rating, modelval_set$rating)
  
  return(RMSE(predicted_rating, modelval_set$rating))
})

qplot(lambdas, rmses)

best_lambda <- lambdas[which.min(rmses)]
print(paste("Best lambda = " , as.character(best_lambda)))
model_base_rmse <- min(rmses)

print("First Extension of Base Model:  adding effect for age of movie at rating")
print("rating ~ movieId + userId + age_at_rating")
print(paste("Provides RMSE = " , as.character(model_base_rmse)))
#  returns RMSE = 0.864257228437467 
# improvement of apprx 0.00054



###########################################################
#
#  2nd EXTENDING BASELINE MODEL TO USE NEW FEATURES 
#
# adding "pre_half_star" indicator
#
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###


lambdas <- seq(4.5, 5.25, 0.25)

rmses <- sapply(lambdas, function(l){
  print( paste("fitting lambda =", as.character(l)))
  
  mu <- mean(train_set$rating)
  
  # movie effect
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  # user effect
  b_u <- train_set %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  # age at rating effect
  b_a <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by= "userId") %>%
    group_by(age_at_rating) %>%
    summarize(b_a = sum(rating - b_u - b_i - mu)/(n()+l))
  
  # pre half star effect
  b_phs <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by= "userId") %>%
    left_join(b_a, by= "age_at_rating") %>%
    group_by(pre_half_star) %>%
    summarize(b_phs = sum(rating - b_a - b_u - b_i - mu)/(n()+l))
  
  
  predicted_rating <-    modelval_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_a, by = "age_at_rating") %>%
    left_join(b_phs, by = "pre_half_star") %>%
    mutate(pred = mu + b_i + b_u + b_a + b_phs) %>%
    pull(pred)
  
  model_rmse <- RMSE(predicted_rating, modelval_set$rating)
  
  return(RMSE(predicted_rating, modelval_set$rating))
})

# check we captchured min
qplot(lambdas, rmses)

best_lambda <- lambdas[which.min(rmses)]
print(paste("Best lambda = " , as.character(best_lambda)))
model_base_rmse <- min(rmses)

print("Second Extension of Base Model:  adding effect for pre_half_star rating indicator")
print("rating ~ movieId + userId + age_at_rating + pre_half_star")
print(paste("Provides RMSE = " , as.character(model_base_rmse)))
#  returns RMSE = 0.86425740853193
# this didn't improve performance much at all
# the pre_half_star feature was excluded from future model use.




###########################################################
#
#  3rd EXTENDING BASELINE MODEL TO USE NEW FEATURES 
#
# using age and genres count at review
#
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

lambdas <- seq(4.5, 5.25, 0.25)

rmses <- sapply(lambdas, function(l){
  
  print( paste("fitting lambda =", as.character(l)))
  
  mu <- mean(train_set$rating)
  
  # movie effect
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  # user effect
  b_u <- train_set %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  # age at rating effect
  b_a <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by= "userId") %>%
    group_by(age_at_rating) %>%
    summarize(b_a = sum(rating - b_u - b_i - mu)/(n()+l))
  
  # genres count effect
  b_gc <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by= "userId") %>%
    left_join(b_a, by= "age_at_rating") %>%
    group_by(genres_count) %>%
    summarize(b_gc = sum(rating - b_a - b_u - b_i - mu)/(n()+l))
  
  predicted_rating <-    modelval_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_a, by = "age_at_rating") %>%
    left_join(b_gc, by = "genres_count") %>%
    mutate(pred = mu + b_i + b_u + b_a + b_gc ) %>%
    pull(pred)
  
  model_rmse <- RMSE(predicted_rating, modelval_set$rating)
  
  return(RMSE(predicted_rating, modelval_set$rating))
})

# check we captchured min
qplot(lambdas, rmses)

best_lambda <- lambdas[which.min(rmses)]
print(paste("Best lambda = " , as.character(best_lambda)))
model_base_rmse <- min(rmses)

print("3rd Extension of Base Model:  adding effect for age at rating and # genres listed for movie")
print("rating ~ movieId + userId + age_at_rating + genres_count")
print(paste("Provides RMSE = " , as.character(model_base_rmse)))
#  returns RMSE = 0.864251437436915 
# improvement of approx 0.000005



###########################################################
#
#  4th EXTENDING BASELINE MODEL TO USE NEW FEATURES 
#
# test impact of adding War Genre  to age at review to see impact of adding one genre to model
#
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# using age at review and War indicator
rmses <- sapply(lambdas, function(l){
  
  print( paste("fitting lambda =", as.character(l)))
  
  mu <- mean(train_set$rating)
  
  # movie effect
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  # user effect
  b_u <- train_set %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  # age at rating effect
  b_a <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by= "userId") %>%
    group_by(age_at_rating) %>%
    summarize(b_a = sum(rating - b_u - b_i - mu)/(n()+l))
  
  # War effect
  b_war <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by= "userId") %>%
    left_join(b_a, by= "age_at_rating") %>%
    group_by(War) %>%
    summarize(b_war = sum(rating - b_a - b_u - b_i - mu)/(n()+l))
  
  # age and genres count effect
  predicted_rating <-    modelval_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_a, by = "age_at_rating") %>%
    left_join(b_war, by = "War") %>%
    mutate(pred = mu + b_i + b_u + b_a + b_war ) %>%
    pull(pred)
  
  model_rmse <- RMSE(predicted_rating, modelval_set$rating)
  
  return(RMSE(predicted_rating, modelval_set$rating))
})

qplot(lambdas, rmses)

best_lambda <- lambdas[which.min(rmses)]
print(paste("Best lambda = " , as.character(best_lambda)))
model_base_rmse <- min(rmses)

print("4th Extension of Base Model:  adding effect for age at rating and # genres and War genre indicator")
print("rating ~ movieId + userId + age_at_rating + War")
print(paste("Provides RMSE = " , as.character(model_base_rmse)))
#  returns RMSE = 0.864256693999561 



###########################################################
#
#  5th EXTENDING BASELINE MODEL TO USE NEW FEATURES 
#
# using data discovery approach to picking genres to use
#
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###


lambdas <- seq(4.5, 5.25, 0.25)
rmses <- sapply(lambdas, function(l){
  print( paste("fitting lambda =", as.character(l)))
  
  mu <- mean(train_set$rating)
  
  # movie effect
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  # user effect
  b_u <- train_set %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  # age at rating effect
  b_age <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by= "userId") %>%
    group_by(age_at_rating) %>%
    summarize(b_age = sum(rating - b_u - b_i - mu)/(n()+l))
  
  # genres count effect
  b_gc <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by= "userId") %>%
    left_join(b_age, by= "age_at_rating") %>%
    group_by(genres_count) %>%
    summarize(b_gc = sum(rating - b_age - b_u - b_i - mu)/(n()+l))
  
  # Drama effect
  b_d <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by= "userId") %>%
    left_join(b_age, by= "age_at_rating") %>%
    left_join(b_gc, by= "genres_count") %>%
    group_by(Drama) %>%
    summarize(b_d = sum(rating - b_gc - b_age - b_u - b_i - mu)/(n()+l))
  
  # Comedy effect
  b_com <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by= "userId") %>%
    left_join(b_age, by= "age_at_rating") %>%
    left_join(b_gc, by= "genres_count") %>%
    left_join(b_d, by= "Drama") %>%
    group_by(Comedy) %>%
    summarize(b_com = sum(rating - b_d - b_gc - b_age - b_u - b_i - mu)/(n()+l))
  
  # Action effect
  b_act <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by= "userId") %>%
    left_join(b_age, by= "age_at_rating") %>%
    left_join(b_gc, by= "genres_count") %>%
    left_join(b_d, by= "Drama") %>%
    left_join(b_com, by= "Comedy") %>%
    group_by(Action) %>%
    summarize(b_act = sum(rating - b_com - b_d - b_gc - b_age - b_u - b_i - mu)/(n()+l))
  
  # Crime effect
  b_crm <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by= "userId") %>%
    left_join(b_age, by= "age_at_rating") %>%
    left_join(b_gc, by= "genres_count") %>%
    left_join(b_d, by= "Drama") %>%
    left_join(b_com, by= "Comedy") %>%
    left_join(b_act, by= "Action") %>%
    group_by(Crime) %>%
    summarize(b_crm = sum(rating - b_act - b_com - b_d - b_gc - b_age - b_u - b_i - mu)/(n()+l))
  
  # SciFi effect
  b_sf <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by= "userId") %>%
    left_join(b_age, by= "age_at_rating") %>%
    left_join(b_gc, by= "genres_count") %>%
    left_join(b_d, by= "Drama") %>%
    left_join(b_com, by= "Comedy") %>%
    left_join(b_act, by= "Action") %>%
    left_join(b_crm, by= "Crime") %>%
    group_by(SciFi) %>%
    summarize(b_sf = sum(rating - b_crm - b_act - b_com - b_d - b_gc - b_age - b_u - b_i - mu)/(n()+l))
  
  # Horror effect
  b_hor <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by= "userId") %>%
    left_join(b_age, by= "age_at_rating") %>%
    left_join(b_gc, by= "genres_count") %>%
    left_join(b_d, by= "Drama") %>%
    left_join(b_com, by= "Comedy") %>%
    left_join(b_act, by= "Action") %>%
    left_join(b_crm, by= "Crime") %>%
    left_join(b_sf, by= "SciFi") %>%
    group_by(Horror) %>%
    summarize(b_hor = sum(rating - b_sf - b_crm - b_act - b_com - b_d - b_gc - b_age - b_u - b_i - mu)/(n()+l))
  
  # War effect
  b_war <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by= "userId") %>%
    left_join(b_age, by= "age_at_rating") %>%
    left_join(b_gc, by= "genres_count") %>%
    left_join(b_d, by= "Drama") %>%
    left_join(b_com, by= "Comedy") %>%
    left_join(b_act, by= "Action") %>%
    left_join(b_crm, by= "Crime") %>%
    left_join(b_sf, by= "SciFi") %>%
    left_join(b_hor, by= "Horror") %>%
    group_by(War) %>%
    summarize(b_war = sum(rating- b_hor - b_sf - b_crm - b_act - b_com - b_d - b_gc - b_age - b_u - b_i - mu)/(n()+l))
  
  
  
  # predict on model validation set (subset of edx dataset)
  predicted_rating <-    modelval_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_age, by = "age_at_rating") %>%
    left_join(b_gc, by = "genres_count") %>%
    left_join(b_d, by= "Drama") %>%
    left_join(b_com, by= "Comedy") %>%
    left_join(b_act, by= "Action") %>%
    left_join(b_crm, by= "Crime") %>%
    left_join(b_sf, by= "SciFi") %>%
    left_join(b_hor, by= "Horror") %>%
    left_join(b_war, by= "War") %>%
    mutate(pred = mu + b_i + b_u + b_age + b_gc + b_d+ b_com + b_act + b_crm + b_sf + b_hor + b_war) %>%
    pull(pred)
  
  model_rmse <- RMSE(predicted_rating, modelval_set$rating)
  
  return(RMSE(predicted_rating, modelval_set$rating))
})

# check we captchured the min
qplot(lambdas, rmses)

best_lambda <- lambdas[which.min(rmses)]
print(paste("Best lambda = " , as.character(best_lambda)))

model_base_rmse <- min(rmses)

print("5th and Final Extension of Base Model:  adding effect for age at rating and # genres and indicators for most predictive genres")
print("rating ~ movieId + userId + age_at_rating + genres_count + Drama + Comedy + Action + Crime + SciFi + Horror + War")
print(paste("Provides RMSE = " , as.character(model_base_rmse)))
# 0.864174922693606
# this is well below best RMSE target of 0.86490 given in project description
# 
# as this will likely be the final model chosen, save the best lambda
final_model_lamba <- best_lambda




###########################################################
#
#  6th test EXTENDING BASELINE MODEL TO USE NEW FEATURES 
#
# testing to see impact if FilmNoir is used in place of Crime
# this was run as a test since FilmNoir had a very large 
# variance from the overall rating average, but also applied to a smaller 
# number of movies.
# Also, this did test to some extent a possible overlap in predictive power between
# the Crime and Drame genres.  It seemed likely that most all Crime movies are listed
# as Drama also, suggesting that removing Crime would not reduce RMSE much.
#
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###



lambdas <- seq(4.5, 5.25, 0.25)
rmses <- sapply(lambdas, function(l){
  print( paste("fitting lambda =", as.character(l)))
  
  mu <- mean(train_set$rating)
  
  # movie effect
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  # user effect
  b_u <- train_set %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  # age at rating effect
  b_age <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by= "userId") %>%
    group_by(age_at_rating) %>%
    summarize(b_age = sum(rating - b_u - b_i - mu)/(n()+l))
  
  # genres count effect
  b_gc <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by= "userId") %>%
    left_join(b_age, by= "age_at_rating") %>%
    group_by(genres_count) %>%
    summarize(b_gc = sum(rating - b_age - b_u - b_i - mu)/(n()+l))
  
  # Drama effect
  b_d <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by= "userId") %>%
    left_join(b_age, by= "age_at_rating") %>%
    left_join(b_gc, by= "genres_count") %>%
    group_by(Drama) %>%
    summarize(b_d = sum(rating - b_gc - b_age - b_u - b_i - mu)/(n()+l))
  
  # Comedy effect
  b_com <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by= "userId") %>%
    left_join(b_age, by= "age_at_rating") %>%
    left_join(b_gc, by= "genres_count") %>%
    left_join(b_d, by= "Drama") %>%
    group_by(Comedy) %>%
    summarize(b_com = sum(rating - b_d - b_gc - b_age - b_u - b_i - mu)/(n()+l))
  
  # Action effect
  b_act <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by= "userId") %>%
    left_join(b_age, by= "age_at_rating") %>%
    left_join(b_gc, by= "genres_count") %>%
    left_join(b_d, by= "Drama") %>%
    left_join(b_com, by= "Comedy") %>%
    group_by(Action) %>%
    summarize(b_act = sum(rating - b_com - b_d - b_gc - b_age - b_u - b_i - mu)/(n()+l))
  
  # FilmNoir effect
  b_fn <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by= "userId") %>%
    left_join(b_age, by= "age_at_rating") %>%
    left_join(b_gc, by= "genres_count") %>%
    left_join(b_d, by= "Drama") %>%
    left_join(b_com, by= "Comedy") %>%
    left_join(b_act, by= "Action") %>%
    group_by(FilmNoir) %>%
    summarize(b_fn = sum(rating - b_act - b_com - b_d - b_gc - b_age - b_u - b_i - mu)/(n()+l))
  
  # SciFi effect
  b_sf <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by= "userId") %>%
    left_join(b_age, by= "age_at_rating") %>%
    left_join(b_gc, by= "genres_count") %>%
    left_join(b_d, by= "Drama") %>%
    left_join(b_com, by= "Comedy") %>%
    left_join(b_act, by= "Action") %>%
    left_join(b_fn, by= "FilmNoir") %>%
    group_by(SciFi) %>%
    summarize(b_sf = sum(rating - b_fn - b_act - b_com - b_d - b_gc - b_age - b_u - b_i - mu)/(n()+l))
  
  # Horror effect
  b_hor <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by= "userId") %>%
    left_join(b_age, by= "age_at_rating") %>%
    left_join(b_gc, by= "genres_count") %>%
    left_join(b_d, by= "Drama") %>%
    left_join(b_com, by= "Comedy") %>%
    left_join(b_act, by= "Action") %>%
    left_join(b_fn, by= "FilmNoir") %>%
    left_join(b_sf, by= "SciFi") %>%
    group_by(Horror) %>%
    summarize(b_hor = sum(rating - b_sf - b_fn - b_act - b_com - b_d - b_gc - b_age - b_u - b_i - mu)/(n()+l))
  
  # War effect
  b_war <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by= "userId") %>%
    left_join(b_age, by= "age_at_rating") %>%
    left_join(b_gc, by= "genres_count") %>%
    left_join(b_d, by= "Drama") %>%
    left_join(b_com, by= "Comedy") %>%
    left_join(b_act, by= "Action") %>%
    left_join(b_fn, by= "FilmNoir") %>%
    left_join(b_sf, by= "SciFi") %>%
    left_join(b_hor, by= "Horror") %>%
    group_by(War) %>%
    summarize(b_war = sum(rating- b_hor - b_sf - b_fn - b_act - b_com - b_d - b_gc - b_age - b_u - b_i - mu)/(n()+l))
  
  
  
  # predict on model validation set (subset of edx dataset)
  predicted_rating <-    modelval_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_age, by = "age_at_rating") %>%
    left_join(b_gc, by = "genres_count") %>%
    left_join(b_d, by= "Drama") %>%
    left_join(b_com, by= "Comedy") %>%
    left_join(b_act, by= "Action") %>%
    left_join(b_fn, by= "FilmNoir") %>%
    left_join(b_sf, by= "SciFi") %>%
    left_join(b_hor, by= "Horror") %>%
    left_join(b_war, by= "War") %>%
    mutate(pred = mu + b_i + b_u + b_age + b_gc + b_d+ b_com + b_act + b_fn + b_sf + b_hor + b_war) %>%
    pull(pred)
  
  model_rmse <- RMSE(predicted_rating, modelval_set$rating)
  
  return(RMSE(predicted_rating, modelval_set$rating))
})

qplot(lambdas, rmses)

best_lambda <- lambdas[which.min(rmses)]
print(paste("Best lambda = " , as.character(best_lambda)))

model_base_rmse <- min(rmses)

print("6th Test Model:  Using 5th extension model but replacing Crime with FilmNoir genre indicator")
print("rating ~ movieId + userId + age_at_rating + genres_count + Drama + Comedy + Action + FilmNoir + SciFi + Horror + War")
print(paste("Provides RMSE = " , as.character(model_base_rmse)))
# 0.864183973570795

# FINDING: replacing Crime with FilmNoir increases RMSE, reject model






###########################################################
#
#  FITTING NEW MODELS TO SEE IF SOMETHING OTHER THAN REGULARIZED LINEAR REGRESSION OUTPERFORMS
#
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###


# NOTE: I tried to fit a randome forest model but it would not run on my computer since it needed very large resources
#       given the size of dataset..
#
# library(randomForest)
# 
# fit <- randomForest(rating~ movieId + userId + genres_count + age_at_rating , data = train_set,
#                     ntres = 50)


# use caret train to find best value of complexity parameter for tree
# set.seed(1234)  
# 
# tree_tune <- train(rating~.,
#                    method = "rpart",
#                    tuneGrid = data.frame(cp = seq(0, 0.1, len = 100)),
#                    data = train_set)
# 
# fit_tree <- rpart(rating ~ movieId + userId + genres_count + age_at_rating +
#                     Comedy + Action + Romance + Crime
#              , data = train_set, method = "anova", 
#              control = rpart.control(minbucket = 1000, cp = .01, maxdepth = 10))


# Trying a simple regression tree
#
# as a test case, use complexity parameter cp=0.001, meaning RMSE must decrease by 0.001 in order to create tree split
# given size of dateset, limit bucket size to 100,000 before split is considered and limit depth to 10 levels
# NOTE: dropping fields for rating timestamp, title and genres list as these are either redundant or appear too complex
fit_tree <- rpart(rating ~ . , data = train_set[, -c(4:6)], method = "anova", 
                  control = rpart.control(minbucket = 100000, cp = .001, maxdepth = 10))
tree_pred <- predict(fit_tree, modelval_set)
RMSE(tree_pred, modelval_set$rating) # returned 1.026205
# this modelreturned 0.9466186 if you include the rating timestamp, title and genres list as predictors, but this doesn't make senses
# finding, tree performance not even close to regularized regression, reject tree without looking at further tuning








###########################################################
#
#  FITTING Final Model on full edx dataset 
#
# final model chosen was # 5 above and used the model form
# "rating ~ movieId + userId + age_at_rating + genres_count + Drama + Comedy + Action + Crime + SciFi + Horror + War"
#
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

l <- final_model_lamba

mu <- mean(edx$rating)

# movie effect
b_i <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+l))

# user effect
b_u <- edx %>%
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+l))

# age at rating effect
b_age <- edx %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by= "userId") %>%
  group_by(age_at_rating) %>%
  summarize(b_age = sum(rating - b_u - b_i - mu)/(n()+l))

# genres count effect
b_gc <- edx %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by= "userId") %>%
  left_join(b_age, by= "age_at_rating") %>%
  group_by(genres_count) %>%
  summarize(b_gc = sum(rating - b_age - b_u - b_i - mu)/(n()+l))

# Drama effect
b_d <- edx %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by= "userId") %>%
  left_join(b_age, by= "age_at_rating") %>%
  left_join(b_gc, by= "genres_count") %>%
  group_by(Drama) %>%
  summarize(b_d = sum(rating - b_gc - b_age - b_u - b_i - mu)/(n()+l))

# Comedy effect
b_com <- edx %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by= "userId") %>%
  left_join(b_age, by= "age_at_rating") %>%
  left_join(b_gc, by= "genres_count") %>%
  left_join(b_d, by= "Drama") %>%
  group_by(Comedy) %>%
  summarize(b_com = sum(rating - b_d - b_gc - b_age - b_u - b_i - mu)/(n()+l))

# Action effect
b_act <- edx %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by= "userId") %>%
  left_join(b_age, by= "age_at_rating") %>%
  left_join(b_gc, by= "genres_count") %>%
  left_join(b_d, by= "Drama") %>%
  left_join(b_com, by= "Comedy") %>%
  group_by(Action) %>%
  summarize(b_act = sum(rating - b_com - b_d - b_gc - b_age - b_u - b_i - mu)/(n()+l))

# Crime effect
b_crm <- edx %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by= "userId") %>%
  left_join(b_age, by= "age_at_rating") %>%
  left_join(b_gc, by= "genres_count") %>%
  left_join(b_d, by= "Drama") %>%
  left_join(b_com, by= "Comedy") %>%
  left_join(b_act, by= "Action") %>%
  group_by(Crime) %>%
  summarize(b_crm = sum(rating - b_act - b_com - b_d - b_gc - b_age - b_u - b_i - mu)/(n()+l))

# SciFi effect
b_sf <- edx %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by= "userId") %>%
  left_join(b_age, by= "age_at_rating") %>%
  left_join(b_gc, by= "genres_count") %>%
  left_join(b_d, by= "Drama") %>%
  left_join(b_com, by= "Comedy") %>%
  left_join(b_act, by= "Action") %>%
  left_join(b_crm, by= "Crime") %>%
  group_by(SciFi) %>%
  summarize(b_sf = sum(rating - b_crm - b_act - b_com - b_d - b_gc - b_age - b_u - b_i - mu)/(n()+l))

# Horror effect
b_hor <- edx %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by= "userId") %>%
  left_join(b_age, by= "age_at_rating") %>%
  left_join(b_gc, by= "genres_count") %>%
  left_join(b_d, by= "Drama") %>%
  left_join(b_com, by= "Comedy") %>%
  left_join(b_act, by= "Action") %>%
  left_join(b_crm, by= "Crime") %>%
  left_join(b_sf, by= "SciFi") %>%
  group_by(Horror) %>%
  summarize(b_hor = sum(rating - b_sf - b_crm - b_act - b_com - b_d - b_gc - b_age - b_u - b_i - mu)/(n()+l))

# War effect
b_war <- edx %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by= "userId") %>%
  left_join(b_age, by= "age_at_rating") %>%
  left_join(b_gc, by= "genres_count") %>%
  left_join(b_d, by= "Drama") %>%
  left_join(b_com, by= "Comedy") %>%
  left_join(b_act, by= "Action") %>%
  left_join(b_crm, by= "Crime") %>%
  left_join(b_sf, by= "SciFi") %>%
  left_join(b_hor, by= "Horror") %>%
  group_by(War) %>%
  summarize(b_war = sum(rating- b_hor - b_sf - b_crm - b_act - b_com - b_d - b_gc - b_age - b_u - b_i - mu)/(n()+l))





###########################################################
#
#  Creaete new features on validation dataset to prepare for final model performance test
#
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

validation <- validation %>% mutate(genres_count = sapply(genres, function(c){length(str_split(c,"\\|", simplify = TRUE))}))

# extract year of movie release from data field "title" in dataset
#NOTE: this takes a few minutes to run.
validation <- validation %>% mutate(movie_year = as.numeric(  str_sub(  str_extract(title,"\\(\\d{4}\\)") , 2, 5)))


# extract year of rating

validation <- validation %>% mutate(rating_date = as.POSIXct(timestamp, origin ='1970-01-01') )
validation <- validation %>% mutate(rating_year = year(rating_date))


# create feature approximating age of movie at time of rating
validation <- mutate(validation, age_at_rating = rating_year - movie_year)


# data checks on new Features
min(validation$age_at_rating)
max(validation$age_at_rating)
table(validation$age_at_rating)

validation$age_at_rating[validation$age_at_rating < 0] <- 0


table(validation$age_at_rating)
summary(validation$age_at_rating)



table(validation$genres_count)


validation <- mutate( validation,
               Action = as.integer( str_detect(genres, "Action")),
               Adventure = as.integer( str_detect(genres, "Adventure")),
               Animation = as.integer( str_detect(genres, "Animation")),
               Children = as.integer( str_detect(genres, "Children")),
               Comedy = as.integer( str_detect(genres, "Comedy")),
               Crime = as.integer( str_detect(genres, "Crime")),
               Documentary = as.integer( str_detect(genres, "Documentary")),
               Drama = as.integer( str_detect(genres, "Drama")),
               Fantasy = as.integer( str_detect(genres, "Fantasy")),
               FilmNoir = as.integer( str_detect(genres, "Film-Noir")),
               Horror = as.integer( str_detect(genres, "Horror")),
               Musical = as.integer( str_detect(genres, "Musical")),
               Mystery = as.integer( str_detect(genres, "Mystery")),
               Romance = as.integer( str_detect(genres, "Romance")),
               SciFi = as.integer( str_detect(genres, "Sci-Fi")),
               Thriller = as.integer( str_detect(genres, "Thriller")),
               War = as.integer( str_detect(genres, "War")),
               Western = as.integer( str_detect(genres, "Western"))
)


# do check on new features created to look for consistency with those in edx dataset
# and look for odd values
summary(validation)  # looks OK.


###########################################################
#
#  Test final model on Validation set provided in project description
#
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###


# predict on final validation set (seperate from  edx dataset)

predicted_rating <-    validation %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_age, by = "age_at_rating") %>%
  left_join(b_gc, by = "genres_count") %>%
  left_join(b_d, by= "Drama") %>%
  left_join(b_com, by= "Comedy") %>%
  left_join(b_act, by= "Action") %>%
  left_join(b_crm, by= "Crime") %>%
  left_join(b_sf, by= "SciFi") %>%
  left_join(b_hor, by= "Horror") %>%
  left_join(b_war, by= "War") %>%
  mutate(pred = mu + b_i + b_u + b_age + b_gc + b_d+ b_com + b_act + b_crm + b_sf + b_hor + b_war) %>%
  pull(pred)


final_rmse <- RMSE(predicted_rating, validation$rating) #0.8642564 which meets best standard given in project definition

print("Final Model Results")
print("Final model uses regularized regresssion")
print("with independent variables for movie effect, user effect, age at rating, # genres and indicators for most predictive genres")
print("rating ~ movieId + userId + age_at_rating + genres_count + Drama + Comedy + Action + Crime + SciFi + Horror + War")
print(paste("Provides RMSE = " , as.character(final_rmse)))


