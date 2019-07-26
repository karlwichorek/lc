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

# Read in the Lending Club data
lc2007 <- read.csv(here("data", "LoanStats3a_securev1.csv"), header=TRUE, skip=1)
lc <- data.frame(lc2007)

###############################################################################
# VARIABLES TO DROP
# NOTE: Loans that were previously not available to individual investors 
# are listed under the "Loans that do not meet the credit policy" header. 
# I am deleting these loans from the dataset.
lc <- lc[-which(lc$id=="Loans that do not meet the credit policy"):-nrow(lc), ]

# Get rid of variables that are unnecessary
var_to_drop <- c("id", "member_id", "url")
lc <- lc[, ! (names(lc) %in% var_to_drop)]

# Distance from a major wealth center could be interesting...
# ...But we can probably just drop zip code for now
# Get rid of employer title as well
na_var <- c("zip_code", "emp_title")
lc <- lc[, ! (names(lc) %in% na_var)]

# These variables have zero variance
zer_var <- c("pymnt_plan", "initial_list_status", 
             "collections_12_mths_ex_med", "policy_code")
lc <- lc[, ! (names(lc) %in% zer_var)]

# These variables are either blank or mostly blank
blk_var <- c("mths_since_last_record", "mths_since_last_major_derog", 
             "mths_since_last_delinq")
lc <- lc[, ! (names(lc) %in% blk_var)]

# Some variables you would not be able to know about prior to making
# an investment decision
uk_var <- c("funded_amnt_inv", "out_prncp", 
            "out_prncp_inv", "total_pymnt", "total_pymnt_inv", 
            "recoveries", "collection_recovery_fee", "last_pymnt_d", 
            "last_pymnt_amnt", "next_pymnt_d", "last_credit_pull_d", 
            "last_fico_range_high", "last_fico_range_low")
lc <- lc[, ! (names(lc) %in% uk_var)]

# Get rid of the two entries where the FICO high ranges are 629 and 634...
# Not sure why those entries are in there as LC didn't allow credit scores
# below 640 to get loans originally.
lc <- lc[! lc$fico_range_high %in% c(629, 634), ]

# Due to a change in underwriting standards, I'm only going to look at
# loans that were originated after Oct 2008. Also dropping anything 
# before December 2008, because there aren't that many loans and they 
# have an excessively high default rate
lc$issue_d <- as.Date(as.yearmon(lc$issue_d, "%b-%Y"))
lc <- subset(lc, lc$issue_d >= '2008-11-30')

# FICO range has a constant difference of 4, 
# so we just need to keep one of the columns
lc$fico_range_low <- NULL

# Drop earliest credit line variable as the credit age variable should
# already be accounted for in the FICO score
lc$earliest_cr_line <- NULL

# Convert revolving utility variable to a number
lc$revol_util <- str_replace_all(lc$revol_util, "[%]", "")
lc$revol_util <- as.numeric(lc$revol_util)

# Drop the funded_amnt variable as almost all loans in the data set were
# fully funded
lc$funded_amnt <- NULL

# Pretty sure I can drop title since it should be contained within the purpose
# variable
lc$title <- NULL

# Drop the description field because it looks like loan descriptions were
# modified after the initial loan listing
# COME BACK TO THIS BECAUSE OTHER ANALYSIS SUGGESTS THIS COULD BE SIGNIFICANT
lc$desc <- NULL

# Don't think income verification will be important because the data is
# winsorized and I don't think the public records variable will be important
# either. 
# Taking out home ownership because none of the models identify it
# as an important variable. 
# Also taking out delinquincies and inquiries as
# those should have affected the FICO score.
drop_add <- c("is_inc_v", "pub_rec", "home_ownership", 
              "delinq_2yrs", "inq_last_6mths", "issue_d")
lc <- lc[, ! (names(lc) %in% drop_add)]

# Drop int_rate, installment, and sub_grade because those are based on the
# same data that you will be using to predict defaults
lc_var <- c("int_rate", "installment", "sub_grade", "grade")
lc <- lc[, ! (names(lc) %in% lc_var)]

###############################################################################
# VARIABLES TO CREATE AND ADDITIONAL VARIABLES TO DROP
# Create a variable to capture the loans that were paid in full or late on
# payments at some point. Consider those loans as defaulted loans because
# when you get to that point in time you have no (or almost no) way of knowing 
# if they will ever pay back the loan. Drop any loans that are current because
# you do not know if they will default in the future.
lc <- lc[! lc$loan_status %in% "Current", ]
lc$default <- ifelse(lc$loan_status=="Fully Paid" & lc$total_rec_late_fee==0, 
                     0, 1)

# Can delete these variables once the loan default variable is created
def_var <- c("loan_status", "total_rec_prncp", "total_rec_int", 
             "total_rec_late_fee")
lc <- lc[, ! (names(lc) %in% def_var)]

# Recategorize the states into regions.
reg_ne <- c("CT", "ME", "MA", "NH", "RI", "VT")
reg_me <- c("DE", "DC", "MD", "NJ", "NY", "PA")
reg_gl <- c("IL", "IN", "MI", "OH", "WI")
reg_pl <- c("IA", "KS", "MN", "MO", "NE", "ND", "SD")
reg_se <- c("AL", "AR", "FL", "GA", "KY", "LA", "MS", "NC", "SC", "TN", "VA", 
            "WV")
reg_sw <- c("AZ", "NM", "OK", "TX")
reg_rm <- c("CO", "ID", "MT", "UT", "WY")
reg_fw <- c("AK", "CA", "HI", "NV", "OR", "WA")
lc$region <- as.character(lc$addr_state)
lc$region[which(lc$region %in% reg_ne)] <- "reg_ne"
lc$region[which(lc$region %in% reg_me)] <- "reg_me"
lc$region[which(lc$region %in% reg_gl)] <- "reg_gl"
lc$region[which(lc$region %in% reg_pl)] <- "reg_pl"
lc$region[which(lc$region %in% reg_se)] <- "reg_se"
lc$region[which(lc$region %in% reg_sw)] <- "reg_sw"
lc$region[which(lc$region %in% reg_rm)] <- "reg_rm"
lc$region[which(lc$region %in% reg_fw)] <- "reg_fw"
lc$region <- as.factor(lc$region)

# Drop the state variable
lc$addr_state <- NULL

# Winsorize variables
win_var <- c("annual_inc", "revol_bal", "total_acc")
lc[, win_var] <- sapply(X=lc[, win_var], FUN=winsor, trim=0.01, na.rm=FALSE)

# Impute missing values for revol_util with the median
lc[is.na(lc[, "revol_util"]), "revol_util"] <- median(lc[, "revol_util"], 
                                                      na.rm=TRUE)

# Impute missing values for emp_length
# Based on some quick data exploration, it doesn't look like 
# the sample of the data where emp_length is "n/a" is any different than the
# rest of the sample data 
lc$emp_length[lc$emp_length == "n/a"] <- "5 years"
lc$emp_length <- factor(lc$emp_length)

# Convert loan term to a numeric variable
lc$term_num <- lc$term
lc$term_num <- as.numeric(str_replace_all(lc$term_num, " months", ""))
lc$term_num <- lc$term_num / 12

# Create a new estimated debt burden variable
lc$db_with_loan <- ((lc$loan_amnt / lc$term_num) + lc$revol_bal) / lc$annual_inc
lc$db_with_loan <- winsor(lc$db_with_loan, trim=0.005)
lc$term_num <- NULL

# Save lending club data
saveRDS(here("output"), lc)

###############################################################################
# Create sample and divide training and testing data
set.seed(8000)
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

# Doesn't work yet
# nn_predict <- compute(creditnet, lc_test[-match("default", names(lc_test))])
