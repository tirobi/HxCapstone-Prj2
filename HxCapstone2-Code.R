###########################################################
#
#  R code showing analysis work performed by Tim Bishop
#  as part of the Hx Data Science Capstone module 
#  Choose Your Own Project 
#
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###




###########################################################
#
# initial setup
# 
#
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# load packages used, download and install if necessary

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(purrr)) install.packages("purrr", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
if(!require(pROC)) install.packages("pROC", repos = "http://cran.us.r-project.org")
if(!require(MASS)) install.packages("MASS", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")



library(tidyverse)
library(caret)
library(data.table)
library(purrr)
library(plotly)
library(xgboost)
library(rpart)
library(rpart.plot)
library(pROC)
library(MASS)
library(randomForest)


# create subdirectory for report plots and files
if (!file.exists("reportfiles")){
  dir.create(file.path(".", "reportfiles"))
}


###########################################################
#
# Load Data  
# download dataset if it is not already in working directory
#
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# check if data file exists in working directory, if not then download it
if (!file.exists("parkinsons.data") ){
  #dl <- tempfile()
  download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/parkinsons/parkinsons.data", "parkinsons.data")
 
}

# read data file
data_original <- read.csv("parkinsons.data",header = TRUE)



###########################################################
#
# Basic Initial Data Validation 
#
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# using basic summary stats to assess data provided 
sum_data <- summary(data_original)
sum_data

# Observations:  there are not missing values, it can't be seen from this if there are outlier
# "status" is the indicator which will be the dependent variable: 1=Parkinsons, 0=Healthy





###########################################################
#
# Split Dataset Into Training, Model Validation and Final Testing Subsets
#
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# split all data into test and training sets
set.seed(2021, sample.kind = "Rounding")    
test_index <- createDataPartition(data_original$status,             # dependent variabls to split on
                                  times = 1,     # how many partitions / splits to return
                                  p = 0.1,       # % of data in test
                                  list = FALSE)  # don't return a list

data_test <- data_original[test_index,]  # true test set which will only be used to test performance of final model selected
data_train <- data_original[-test_index,]

# split all training set into model validation and true training sets
# the model validation set will be used to test model for the purpose of 
# initial model performance, tune checks and comparing performance between 
# models being considered. 

validation_index <- createDataPartition(data_train$status,  # dependent variabls to split on
                                  times = 1,     # how many partitions / splits to return
                                  p = 0.1,       # % of data in test
                                  list = FALSE)  # don't return a list

data_validation <- data_train[validation_index,] # model validation set
data_train <- data_train[-validation_index,]     # final training set 

table(data_train$status) # show imbalance issue in status indicator


# create a second training set which removes the imbalance issue by up sampling
# on the lesser occuring value of the status indicator 
# which is status =0 indicating Healthy
# 
# training on this set will be tested during the modeling process to see if training 
# on this set improves model fit and predictive performance
data_up_train <- upSample(x = data_train,
                     y = factor(data_train[,18]))       
# remove last column called "class" which upSample function has added
data_up_train <- data_up_train[,-ncol(data_up_train)]
table(data_up_train$status)  # show imbalance is gone in this set


# review summary output for train, validation and test sets
# key is to look for distribution of status indicator
sum_train <- summary(data_train)
sum_validation <- summary(data_validation)
sum_test <- summary(data_test)

sum_train 
sum_validation 
sum_test 



# calculate and review proportion of status=1 in various sets.
# all seem reasonably close given dataset sizes.

status_means <- data.frame( Group = "Full Data Set", Status_Mean= mean(data_original$status))
status_means[2,1] <- "Train Set"
status_means[2,2] <-  mean(data_train$status)
status_means[3,1] <- "Model Val Set"
status_means[3,2] <-  mean(data_validation$status)
status_means[4,1] <- "Test Set"
status_means[4,2] <-  mean(data_test$status)
status_means[5,1] <- "Up Sampled Train Set"
status_means[5,2] <-  mean(data_up_train$status)

knitr::kable(status_means) # put in nice table format for Rmd display



###########################################################
#
#  Data Exploration on Training Data 
#
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# Note: to be quite pure in keeping the test dataset independent
# I am only using the training set for performing any data exploration 
# and looking for relationship to the dependent variable 

# view distribution of each variable, where each variable is in a separate column in data
plot_dist1 <- data_train[,2:13] %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

plot_dist2 <- data_train[,14:24] %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()+
  theme_bw()

# save plot to file for use in report
jpeg(filename = "reportfiles/plotdist1.jpg")
plot_dist1
dev.off()

jpeg(filename = "reportfiles/plotdist2.jpg")
plot_dist2
dev.off()



# create plot which one by one show the relationship
# between each independent variable and the dependent variable "status"
plot_rel1 <- data_train[c(2:12,18)] %>% 
  gather(-status, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = status, color = status)) +
  geom_point() +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

# save plot to file
jpeg(filename = "reportfiles/plotrel1.jpg")
plot_rel1
dev.off()


plot_rel2 <- data_train[c(13:24)] %>% 
  gather(-status, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = status, color = status)) +
  geom_point() +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

# save plot to file
jpeg(filename = "reportfiles/plotrel2.jpg")
plot_rel2
dev.off()




# # Let's check the correlations 

# Calculate correlation matrix
cormat <- cor(data_train[-1]) # exclude first column which is a character value for name
# Melt the correlation matrix
melted.cormat <- melt(cormat)
# Remember plotly from the welcome notes?
plot_corr <- plot_ly(
  x = melted.cormat$Var1, y = melted.cormat$Var2,
  z = melted.cormat$value, colorscale = "Reds", type = "heatmap"
)

# show plot
plot_corr


# save plot to file for report
jpeg(filename = "reportfiles/plotcorr.jpg")
plot_ly(
  x = melted.cormat$Var1, y = melted.cormat$Var2,
  z = melted.cormat$value, colorscale = "Reds", type = "heatmap"
)
dev.off()


# Use a Principle Component Analysis to snoop for information or relationships 
# of value in modeling

# for PCA, remove the character string name field, and the dependent variable status field
# PCA is done on numeric independent variable fields only 

# check indexes to exclude are correct
names(data_train[,c(1,18)])


# note PCA results are sensitive to scales of each variable
# This is prevented by standardizing (ie centering and scaling) the matrix first
# using parameters of prcomp to do the standardization 
model_pca <- prcomp(as.matrix(data_train[,-c(1,18)]), center = TRUE, scale. = TRUE)

# look at PCA output summary
sum_pca <- summary(model_pca)
sum_pca


# look at pca rotations to see if particular dependent variables have very heavy weights
model_pca$rotation

# sample rotations for report
knitr::kable( model_pca$rotation[,1:5])


# produce elbow plot
pc_number <- 1:ncol(data_train[,-c(1,18)])
variance_explained <- cumsum(model_pca$sdev^2 / sum(model_pca$sdev^2))

ggplot(data.frame(cbind(x=c(0,pc_number),y=c(0,variance_explained))), aes(x, y)) + 
        geom_point() +
         xlab("PC Numbers") +
         ylab("Cummulative Variance Explained")

# save plot to file for report
jpeg(filename = "reportfiles/pcacumvar.jpg")
ggplot(data.frame(cbind(x=c(0,pc_number),y=c(0,variance_explained))), aes(x, y)) + 
  geom_point() +
  xlab("PC Numbers") +
  ylab("Cummulative Variance Explained")
dev.off()



# set up list to keep results from various models assessed
results <- tibble()

results <- bind_rows(results,
                     tibble(Method = 'Mean Of Validation set status',
                            Accuracy =mean(data_validation$status),
                            Sensitivity = 1.0,
                            Specificity = 0.0,
                            AUC = 0.5))


###########################################################
#
#  MODEL 1
#  simple classification tree
#
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###


# Trying a simple classification tree
#
# NOTE: dropping field for "Name" which is an identifier and not a variable
fit_tree <- rpart(status ~ . , data = data.frame(data_train[-1]), method = "class", 
                  control = rpart.control(minbucket = 5, cp = .00001, maxdepth = 10))

# use fit model to predict on model validation data
pred_tree <- predict(fit_tree, data.frame(data_validation[-1]), type = 'class')

# cal confusion matrix
cm_tree = confusionMatrix(factor(pred_tree, levels = c("1","0"), labels = c("Parkinson", "Healthy")), 
                          factor( data_validation$status, levels = c("1","0"), labels = c("Parkinson", "Healthy")))

# view confusion matrix and explicitly print Accuracy of model
cm_tree
cm_tree$overall["Accuracy"]

# visualize fit tree
rpart.plot(fit_tree)

# save plot to file for report
jpeg(filename = "reportfiles/rpart1.jpg")
rpart.plot(fit_tree)
dev.off()

# cal receiver operator curve, plot curve, save plot to file
roc_tree <- roc( data_validation$status,  as.numeric(pred_tree ))
plot(roc_tree, xlim = c(1,0))
# save plot to file for report
jpeg(filename = "reportfiles/rpart1_roc.jpg")
plot(roc_tree, xlim = c(1,0))
dev.off()

# view Area Under Curve (under ROC curve)
roc_tree$auc

# same model results for comparison later
results <- bind_rows(results,
                     tibble(Method = 'Simple Classification Tree',
                            Accuracy = cm_tree$overall["Accuracy"],
                            Sensitivity = cm_tree$byClass["Sensitivity"],
                            Specificity = cm_tree$byClass["Specificity"],
                            AUC = roc_tree$auc[1]))


###########################################################
#
#   MODEL 2
#  simple classification tree, fit on up sampled data
#
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# TRy tree on upSampled datat
fit_tree_up <- rpart(status ~ . , data = data.frame(data_up_train[-1]), method = "class", 
                  control = rpart.control(minbucket = 5, cp = .00001, maxdepth = 10))
pred_tree_up <- predict(fit_tree_up, data.frame(data_validation[-1]), type = 'class')


cm_tree_up = confusionMatrix(factor(pred_tree_up, levels = c("1","0"), labels = c("Parkinson", "Healthy")), 
                          factor( data_validation$status, levels = c("1","0"), labels = c("Parkinson", "Healthy")))

cm_tree_up
cm_tree_up$overall["Accuracy"]

rpart.plot(fit_tree_up)
# save plot to file for report
jpeg(filename = "reportfiles/rpart2.jpg")
rpart.plot(fit_tree_up)
dev.off()


roc_tree_up <- roc( data_validation$status,  as.numeric(pred_tree_up ))
plot(roc_tree_up, xlim = c(1,0))
roc_tree_up$auc

results <- bind_rows(results,
                     tibble(Method = 'Classification Tree Upsampled Data',
                            Accuracy = cm_tree_up$overall["Accuracy"],
                            Sensitivity = cm_tree_up$byClass["Sensitivity"],
                            Specificity = cm_tree_up$byClass["Specificity"],
                            AUC = roc_tree_up$auc[1]))


###########################################################
#
#  Model 3  Logistic REgression 
#
#
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###


#  Logistic regression model

# create model formula including all independent variable names
glm.f <- as.formula(paste("status ~", paste(colnames(data_train)[-c(1,18)], collapse=" + ")))

#  look at formula to check
glm.f

# fit model
  glm_fit <- glm(glm.f,data=data_train[-1], family=binomial) 
  # note: didn't converge in fitting
  
# look at results summary
summary(glm_fit)

#  backward stepwise not used since glm model didn't converge
# glm_stepAIC <- stepAIC(glm, direction = "backward", k=2) # use AIC for variable removal decisions
# 
# 
# glm_fit <- glm(glm_stepAIC$formula, data=data_train[-1], family=binomial)#(link = "logit"))
# 
# summary(glm_fit)


glm_pred_val <- predict(glm_fit, data_validation[,-1],  type = 'response' ) 

# convert predictions and validation status vectors to factors with levels I want
glm_pred_val <- factor(ifelse(glm_pred_val>=0.5,1,0), levels =  c(1,0), labels = c("Parkinsons","Healthy"))
val_status <- factor(data_validation$status,levels =  c(1,0), labels = c("Parkinsons","Healthy"))

#calc confusion matrix
cm_lr <- confusionMatrix(data = val_status ,
                reference = glm_pred_val)
cm_lr
cm_lr$overall["Accuracy"]

# ROC curve and AUC
lr_roc <- roc(as.numeric(val_status), as.numeric(glm_pred_val))
plot(rf_roc)
lr_roc$auc

results <- bind_rows(results,
                     tibble(Method = 'Logistic Regression (non-converging)',
                            Accuracy = cm_lr$overall["Accuracy"],
                            Sensitivity = cm_lr$byClass["Sensitivity"],
                            Specificity = cm_lr$byClass["Specificity"],
                            AUC = lr_roc$auc[1]))



###########################################################
#
#  Model 4  Logistic REgression fit with forward stepwise regression
#
#
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

#  Stepwise Logistic regression 

# create model formula including all independent variable names
glm.fbig <- as.formula(paste("status ~", paste(colnames(data_train)[-c(1,18)], collapse=" + ")))
#  look at formula to check
glm.fbig

glm.fnone <- as.formula("status ~ 1")
#  look at formula to check
glm.fnone


# fit model base model
glm_fit_fnone <- glm(glm.fnone,data=data_train[-1], family=binomial)


# run stepwise regression
glm_Step = step(glm_fit_fnone, scope = list(upper=glm.fbig, lower=glm.fnone), 
                scale = 0, direction = c('forward'))


# look at results summary
summary(glm_Step)
# look at final formula
glm_Step$formula

glm_step_pred_val <- predict(glm_Step, data_validation[,-1],  type = 'response' ) 

# convert predictions and validation status vectors to factors with levels I want
glm_step_pred_val <- factor(ifelse(glm_step_pred_val>=0.5,1,0), levels =  c(1,0), labels = c("Parkinsons","Healthy"))
val_status <- factor(data_validation$status,levels =  c(1,0), labels = c("Parkinsons","Healthy"))

#calc confusion matrix
cm_lr_step <- confusionMatrix(data = val_status ,
                         reference = glm_step_pred_val)
cm_lr_step
cm_lr_step$overall["Accuracy"]

# ROC curve and AUC
lr_step_roc <- roc(as.numeric(val_status), as.numeric(glm_step_pred_val))
plot(lr_step_roc)
lr_step_roc$auc

results <- bind_rows(results,
                     tibble(Method = 'Forward Stepwise Logistic Regression',
                            Accuracy = cm_lr_step$overall["Accuracy"],
                            Sensitivity = cm_lr_step$byClass["Sensitivity"],
                            Specificity = cm_lr_step$byClass["Specificity"],
                            AUC = lr_step_roc$auc[1]))




###########################################################
#
#  Model 5  Stepwise Logistic regression on up sampled dataset
#
#
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# fit model
glm_fit_fnone <- glm(glm.fnone,data=data_up_train[-1], family=binomial)


glm_up_Step = step(glm_fit_fnone, scope = list(upper=glm.fbig, lower=glm.fnone), 
                scale = 0, direction = c('forward'))


# look at results summary
summary(glm_up_Step)


glm_up_pred_val <- predict(glm_up_Step, data_validation[,-1],  type = 'response' ) 

# convert predictions and validation status vectors to factors with levels I want
glm_up_step_pred_val <- factor(ifelse(glm_up_pred_val>=0.5,1,0), levels =  c(1,0), labels = c("Parkinsons","Healthy"))
val_status <- factor(data_validation$status,levels =  c(1,0), labels = c("Parkinsons","Healthy"))

#calc confusion matrix
cm_lr_up_step <- confusionMatrix(data = val_status ,
                              reference = glm_up_step_pred_val)
cm_lr_up_step
cm_lr_up_step$overall["Accuracy"]

# ROC curve and AUC
lr_up_roc <- roc(as.numeric(val_status), as.numeric(glm_up_step_pred_val))
plot(lr_up_roc)
lr_up_roc$auc

results <- bind_rows(results,
                     tibble(Method = 'Forward Stepwise Logistic Reg with Up Sampling',
                            Accuracy = cm_lr_up_step$overall["Accuracy"],
                            Sensitivity = cm_lr_up_step$byClass["Sensitivity"],
                            Specificity = cm_lr_up_step$byClass["Specificity"],
                            AUC = lr_up_roc$auc[1]))


glm_up_Step$formula






###########################################################
#
#  Model 6  Logistic REgression on PCA transformed data
#
#
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# use PCs to drive logistic regression
glm_pca.f <- as.formula(paste("status ~", paste(colnames(model_pca$x[,1:10]), collapse=" + ")))

glm_pca.f

# using dataframe with first 10 PCs and adding status variable
glm_pca <- glm(glm_pca.f,data=data.frame(cbind(model_pca$x[,1:10],status=data_train[,18])), family=binomial) #(link = "logit"))
summary(glm_pca)

# WIP: explain why using stepAIC, pros and cons
glm_pca_step <- stepAIC(glm_pca, direction = "backward", k=2) # use AIC for variable removal decisions
glm_pca_step$formula
glm_pca_fit <- glm(glm_pca_step$formula, data=data.frame(cbind(model_pca$x[,1:10],status=data_train[,18])), family=binomial)#(link = "logit"))
summary(glm_pca_fit)


# set up scaled, centered and rotated data on validation set
data_val_pca <- scale( data_validation[,-c(1,18)], center = TRUE)

# get predictions
data_val_pca <- predict(model_pca, data_val_pca)



glm_pca_pred_val <- predict(glm_pca_fit, 
                            data.frame(cbind(data_val_pca,status=data_validation[,18])),  
                            type = 'response' ) 

# round to get rid of noise in almost zero values
glm_pca_pred_val <- round(glm_pca_pred_val)

val_status <- factor(data_validation$status,levels =  c(1,0), labels = c("Parkinsons","Healthy"))

cm_pca <- confusionMatrix(data = val_status ,
                reference = factor(glm_pca_pred_val, levels = c(1,0), labels = c("Parkinsons","Healthy")))

cm_pca
cm_pca$overall["Accuracy"]

# ROC curve and AUC
pca_roc <- roc(as.numeric(val_status), as.numeric(glm_pca_pred_val))
plot(pca_roc)
pca_roc$auc


results <- bind_rows(results,
                     tibble(Method = 'PCA with Logistic Regression',
                            Accuracy = cm_pca$overall["Accuracy"],
                            Sensitivity = cm_pca$byClass["Sensitivity"],
                            Specificity = cm_pca$byClass["Specificity"],
                            AUC = pca_roc$auc[1]))





###########################################################
#
#  Model 7 XGBoost 
#
#
#
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###


# Fitting XGBoost to the Training set
fit_xgb = xgboost(data = data.matrix(data_train[,-c(1,18)]), 
                  label = data.matrix(data_train[,18]), 
                  Booster = "gbtree",
                  nrounds = 100)

# Predicting the Test set results
pred_xgb_val = predict(fit_xgb, newdata = data.matrix(data_validation[,-c(1,18)]), method = "class")
pred_xgb_val = as.integer(pred_xgb_val >= 0.5)

# Making the Confusion Matrix
cm_xgb = confusionMatrix(factor(pred_xgb_val, levels = c("1","0"), labels = c("Parkinson", "Healthy")), 
                         factor(data_validation[,18], levels = c("1","0"), labels = c("Parkinson", "Healthy")))
cm_xgb

cm_xgb$overall["Accuracy"]

xgb_roc <- roc(val_status, pred_xgb_val)

results <- bind_rows(results,
                     tibble(Method = 'XGBoost Model',
                            Accuracy = cm_xgb$overall["Accuracy"],
                            Sensitivity = cm_xgb$byClass["Sensitivity"],
                            Specificity = cm_xgb$byClass["Specificity"],
                            AUC = xgb_roc$auc[1]))


# as a test try
# Fitting XGBoost using booster = dart instead of tree based approach
fit_xgb = xgboost(data = data.matrix(data_train[,-c(1,18)]), 
                  label = data.matrix(data_train[,18]), 
                  Booster = "dart", 
                  nrounds = 100)

# Predicting the Test set results
pred_xgb_val = predict(fit_xgb, newdata = data.matrix(data_validation[,-c(1,18)]), method = "class")
pred_xgb_val = as.integer(pred_xgb_val >= 0.5)

# Making the Confusion Matrix
cm_xgb = confusionMatrix(factor(pred_xgb_val, levels = c("1","0"), labels = c("Parkinson", "Healthy")), 
                         factor(data_validation[,18], levels = c("1","0"), labels = c("Parkinson", "Healthy")))
cm_xgb
cm_xgb$overall["Accuracy"]


# test xgboost
# using up trained data

fit_xgb = xgboost(data = data.matrix(data_up_train[,-c(1,18)]), label = data.matrix(data_up_train[,18]), nrounds = 100)

# Predicting the Test set results
pred_xgb_val = predict(fit_xgb, newdata = data.matrix(data_validation[,-c(1,18)]), method = "class")
pred_xgb_val = as.integer(pred_xgb_val >= 0.5)

# Making the Confusion Matrix
#cm = table(y_test, y_pred)
cm_xgb = confusionMatrix(factor(pred_xgb_val, levels = c("1","0"), labels = c("Parkinson", "Healthy")), 
                         factor(data_validation[,18], levels = c("1","0"), labels = c("Parkinson", "Healthy")))
cm_xgb
cm_xgb$overall["Accuracy"]





###########################################################
#
#  Model 8  Random Forest 
#
# 
#
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###


# using cross validation to train for mrty best mtry parameter

set.seed(3021, sample.kind = "Rounding")

rf_control <- trainControl(method="cv", number = 5)
grid <- data.frame(mtry = c(1:ncol(data_train[, -c(1,18)])))
train_rf <- train(data_train[, -c(1,18)], 
                  factor(data_train[,18], levels = c("1","0"), labels = c("Parkinson", "Healthy")),
                  method = "rf",
                  ntree = 100,
                  trControl = rf_control,
                  tuneGrid = grid,
                  nSamp = 5000)

ggplot(train_rf)
# save plot for report
jpeg(filename = "reportfiles/rfmtrytune.jpg")
ggplot(train_rf)
dev.off()

# show best mtry value
train_rf$bestTune


# fit final rf model using best mtry parameter to train set
fit_rf <- randomForest(data_train[, -c(1,18)], 
                       factor(data_train[,18], levels = c("1","0"), labels = c("Parkinson", "Healthy")),
                       minNode = train_rf$bestTune$mtry)

plot(fit_rf)

pred_rf <- predict(fit_rf, data_validation[, -c(1,18)], method = "class")
cm_rf <- confusionMatrix(pred_rf, 
                      factor(data_validation[,18], levels = c("1","0"), labels = c("Parkinson", "Healthy")))
cm_rf
cm_rf$overall["Accuracy"]

rf_roc <- roc(val_status, ifelse(pred_rf==1,1,0))

results <- bind_rows(results,
                     tibble(Method = "Random Forest",
                            Accuracy = cm_rf$overall["Accuracy"],
                            Sensitivity = cm_rf$byClass["Sensitivity"],
                            Specificity = cm_rf$byClass["Specificity"],
                            AUC = rf_roc$auc[1]))



###########################################################
#
#  SElecting  Final Model 
#
#
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# review results table
results

# Rmd friendly version of results
knitr::kable(results, digits = 2)



###########################################################
#
#  Refit selected model on combined train and validation data
#
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###


final_model <- rpart(status ~ . , data = data.frame(data_original[-test_index,-1]), method = "class", 
                  control = rpart.control(minbucket = 5, cp = .00001, maxdepth = 10))


rpart.plot(final_model)



###########################################################
#
#  Test final model on Validation set provided in project description
#
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# use fit model to predict on model validation data
pred_final <- predict(final_model, data.frame(data_test[-1]), type = 'class')

# cal confusion matrix
cm_final = confusionMatrix(factor(pred_final, levels = c("1","0"), labels = c("Parkinson", "Healthy")), 
                          factor( data_test$status, levels = c("1","0"), labels = c("Parkinson", "Healthy")))

# view confusion matrix and explicitly print Accuracy of model
cm_final
cm_final$overall["Accuracy"]


# save plot to file for report
jpeg(filename = "reportfiles/rpart_final.jpg")
rpart.plot(final_model)
dev.off()

# calc receiver operator curve, plot curve, save plot to file
roc_final <- roc( data_test$status,  ifelse(pred_final==1,1,0))
plot(roc_final, xlim = c(1,0))
# save plot to file for report
jpeg(filename = "reportfiles/rpart1_final_roc.jpg")
plot(roc_final, xlim = c(1,0))
dev.off()

# view Area Under Curve (under ROC curve)
roc_final$auc

# final model results
knitr::kable( tibble(Method = 'Final Model - Classification Tree',
                            Accuracy = cm_final$overall["Accuracy"],
                            Sensitivity = cm_final$byClass["Sensitivity"],
                            Specificity = cm_final$byClass["Specificity"],
                            AUC = roc_final$auc[1]),
              digits = 2)



