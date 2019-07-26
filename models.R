# Load packages
library(stringr)
library(catspec)
library(xts)
library(psych)
library(car)
library(caret)
library(gbm)
library(randomForest)
library(pROC)
library(caret)
library(nnet)
library(neuralnet)
library(NeuralNetTools)
library(here)

###############################################################################
# Create sample and divide training and testing data
set.seed(123)

# load data
lc <- loadRDS("lc.rds", here("output"))

trainMe <- createDataPartition(lc$default, p=0.7, list=FALSE, times=1)
lc_train <- lc[trainMe, ]
lc_test <- lc[-trainMe, ]

getResample <- function(training, pctDeadbeat) {
  deadbeat.size <- floor(pctDeadbeat*nrow(training))
  responsible.size <- floor((1-pctDeadbeat)*nrow(training))
  
  training.deadbeat <- training[training$default == 1, ]
  training.responsible <- training[training$default == 0, ]
  
  training <- rbind(training.deadbeat[sample(1:nrow(training.deadbeat),
                                             deadbeat.size,
                                             replace=TRUE),],
                    training.responsible[sample(1:nrow(training.responsible),
                                                responsible.size,
                                                replace=TRUE),])
  training
}

lc_train_bal <- getResample(lc_train, 0.50)

###############################################################################
# GBM
###############################################################################
gbm_model <- gbm(default ~ ., 
                 distribution = "adaboost", 
                 data = lc_train, 
                 n.trees = 700, 
                 interaction.depth = 5, 
                 shrinkage = 0.01, 
                 cv.folds = 10, 
                 verbose = TRUE)

gbm_perf <- gbm.perf(gbm_model, method = "cv")
summary(gbm_model)

# Check out what a cross-validated approach to tuning will deliver for
# gbm parameters. Main parameter that was changed was the interaction depth.
myTuneGrid <- expand.grid(n.trees=700, 
                          interaction.depth=5, 
                          shrinkage=seq(0.001, 0.05, by=0.001))
fitControl <- trainControl(method="repeatedcv", 
                           number=3, 
                           repeats=1, 
                           verboseIter=FALSE)
gbm_train_model <- train(default ~ ., data=lc_train, 
                         method="gbm", trControl=fitControl, 
                         tuneGrid=myTuneGrid, verbose=FALSE)
print(gbm_train_model)
plot(gbm_train_model)

predictions_gbm <- predict.gbm(gbm_model, 
                               lc_test[-match("default", names(lc_test))], 
                               n.trees = 700, type = "response")

g1 <- roc(lc_test$default, predictions_gbm)
plot(g1)

gbm_model_bal <- gbm(default ~ ., 
                     distribution = "adaboost", 
                     data = lc_train_bal, 
                     n.trees = 700, 
                     interaction.depth = 5, 
                     shrinkage = 0.01, 
                     cv.folds = 10, 
                     verbose = TRUE)

gbm_perf_bal <- gbm.perf(gbm_model_bal, method = "cv")
summary(gbm_model_bal)

###############################################################################
# Random Forest
###############################################################################
rf_model <- randomForest(as.factor(default) ~ ., 
                         data = lc_train, 
                         mtry = 2, 
                         importance = TRUE)

importance(rf_model)
varImpPlot(rf_model, type = 1)
print(rf_model)

# Tuned rf model
rf_tuned_model <- train(as.factor(default) ~ ., data=lc_train, method="rf")

print(rf_tuned_model)

rf_predict <- predict(rf_model, lc_test[-match("default", names(lc_test))], 
                      type = "prob")
rf1 <- roc(lc_test$default, rf_predict[, 2])
plot(rf1)

rf_model_bal <- randomForest(as.factor(default) ~ ., 
                             data = lc_train_bal, 
                             importance = TRUE)

importance(rf_model_bal)
varImpPlot(rf_model_bal, type = 1)
print(rf_model_bal)

rf_bal_predict <- predict(rf_model_bal, lc_test[-match("default", names(lc_test))], 
                      type = "prob")
rf2 <- roc(lc_test$default, rf_bal_predict[, 2])
plot(rf2)

qqnorm((RFestimated - ValidationData$V9)/sd(RFestimated - ValidationData$V9))
qqline((RFestimated - ValidationData$V9)/sd(RFestimated - ValidationData$V9))
plot(ValidationData$V9, RFestimated)

###############################################################################
# Neural Net
###############################################################################
creditnet <- train(x = lc_train[-match("default", names(lc_train))], 
                   y = lc_train$default, 
                   method = 'nnet')

creditnet <- nnet(default ~ ., 
                  data = lc_train, 
                  size = 3, 
                  decay  = 0.1, 
                  maxit = 1000)

# plot the NN
plotnet(creditnet)
garson <- garson(creditnet, "default", bar_plot=FALSE)
