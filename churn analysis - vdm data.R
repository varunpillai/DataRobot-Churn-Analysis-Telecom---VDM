# CHURN ANALYSIS OF TELECOM DATA FROM http://www.wiley.com/legacy/compbooks/soukup/downloads.html

#load library
library(Hmisc)
library(rpart)
library(party)
library(caret)
library(pROC)
library(ROCR)
library(randomForest)
library(FSelector)
library(corrgram)

# data exploration
traindata=read.csv(file="traindata.csv")
# attach(traindata)

summary(traindata)
describe(traindata)

corrgram(traindata, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Churn Analysis") 

# file splitting
crs = new.env()

crs$seed = 1234
crs$dataset = traindata

# Build the training/validate/test datasets.
set.seed(crs$seed) 
crs$nobs = nrow(crs$dataset) 
crs$sample = crs$train = sample(nrow(crs$dataset), .8*crs$nobs)
crs$validate = sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.2*crs$nobs) 

crs$input = c("REGION","CONTRACT_FEE","AGE_RANGE", "CREDIT_APPROVAL", "CONTACT_METHOD",
               "RATE_PLAN","TOT_OPEN_AMT","TOT_INVOICE_AMT")

crs$target  = "CHURN"

# train = traindata[train_ind, ]
# test = traindata[-train_ind, ]
train=crs$dataset[crs$train, c(crs$input, crs$target)]
test =crs$dataset[crs$validate, c(crs$input, crs$target)]

# CART

### REGRESSION
fit<-rpart(CHURN~., data=train, method="anova", 
           control=rpart.control(minsplit=50, cp=0.01))

##display complexity parameter table
printcp(fit)

###plot cross-validation results
plotcp(fit)

###detailed results including splits
summary(fit)

# confusion matrix # accuracy: 76.93%
pred <- as.vector(ifelse(predict(fit, newdata=test) > 0.5, "1","0"))
confusionMatrix(pred, test$CHURN)

###Prune the tree to the desired size ..at min error cp
pfit<- prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
summary(pfit)

# confusion matrix # accuracy: 76.93%
pred <- as.vector(ifelse(predict(pfit, newdata=test) > 0.5, "1","0"))
confusionMatrix(pred, test$CHURN)

# Random Forest
fit <- randomForest(as.factor(CHURN)~., data=traindata, importance=TRUE, ntree=100,OOB=TRUE)
fit$terms

varImpPlot(fit)

# confusion matrix # accuracy: 80.64%
pred <- predict(fit, test)
confusionMatrix(pred, test$CHURN)

# --------- code from rattle

library(rattle)

# This log generally records the process of building a model. However, with very 
# little effort the log can be used to score a new dataset. The logical variable 
# 'building' is used to toggle between generating transformations, as when building 
# a model, and simply using the transformations, as when scoring a dataset.

building <- TRUE
scoring  <- ! building

# The colorspace package is used to generate the colours used in plots, if available.

library(colorspace)

# A pre-defined value is used to reset the random seed so that results are repeatable.

crv$seed <- 42 

#============================================================
# Rattle timestamp: 2015-06-01 15:33:20 x86_64-w64-mingw32 

# Load an R data frame.

crs$dataset <- traindata

# Display a simple summary (structure) of the dataset.

str(crs$dataset)

#============================================================
# Rattle timestamp: 2015-06-01 15:33:28 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(0) 
crs$nobs <- nrow(crs$dataset) # 371933 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 260353 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 55789 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 55791 observations

# The following variable selections have been noted.

crs$input <- c("CITY", "STATE", "ZIP", "REGION",
               "CURRENTBALANCE", "CONTRACT_FEE", "TOT_OPEN_AMT", "TOT_INVOICE_AMT",
               "TOT_PAID_AMT", "NUM_INVOICES", "ACTIVATED_YEAR", "ACTIVATED_MONTH",
               "AGE_RANGE", "CREDIT_APPROVAL", "RENEWAL_YEAR", "RENEWAL_MONTH",
               "CONTACT_METHOD", "RATE_PLAN")

crs$numeric <- c("CURRENTBALANCE", "CONTRACT_FEE", "TOT_OPEN_AMT", "TOT_INVOICE_AMT",
                 "TOT_PAID_AMT", "NUM_INVOICES", "ACTIVATED_YEAR", "RENEWAL_YEAR")

crs$categoric <- c("CITY", "STATE", "ZIP", "REGION",
                   "ACTIVATED_MONTH", "AGE_RANGE", "CREDIT_APPROVAL", "RENEWAL_MONTH",
                   "CONTACT_METHOD", "RATE_PLAN")

crs$target  <- "CHURN"
crs$risk    <- NULL
crs$ident   <- c("X", "CUSTOMERID")
crs$ignore  <- NULL
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2015-06-01 15:34:56 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(0) 
crs$nobs <- nrow(crs$dataset) # 371933 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 260353 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 55789 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 55791 observations

# The following variable selections have been noted.

crs$input <- c("REGION", "CURRENTBALANCE", "CONTRACT_FEE", "TOT_OPEN_AMT",
               "TOT_INVOICE_AMT", "TOT_PAID_AMT", "NUM_INVOICES", "ACTIVATED_YEAR",
               "ACTIVATED_MONTH", "AGE_RANGE", "CREDIT_APPROVAL", "RENEWAL_YEAR",
               "RENEWAL_MONTH", "CONTACT_METHOD", "RATE_PLAN")

crs$numeric <- c("CURRENTBALANCE", "CONTRACT_FEE", "TOT_OPEN_AMT", "TOT_INVOICE_AMT",
                 "TOT_PAID_AMT", "NUM_INVOICES", "ACTIVATED_YEAR", "RENEWAL_YEAR")

crs$categoric <- c("REGION", "ACTIVATED_MONTH", "AGE_RANGE", "CREDIT_APPROVAL",
                   "RENEWAL_MONTH", "CONTACT_METHOD", "RATE_PLAN")

crs$target  <- "CHURN"
crs$risk    <- NULL
crs$ident   <- "X"
crs$ignore  <- c("CUSTOMERID", "CITY", "STATE", "ZIP")
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2015-06-01 15:35:34 x86_64-w64-mingw32 

# Random Forest 

# The 'randomForest' package provides the 'randomForest' function.

require(randomForest, quietly=TRUE)

# Build the Random Forest model.

set.seed(crv$seed)
crs$rf <- randomForest(as.factor(CHURN) ~ .,
                       data=crs$dataset[crs$sample,c(crs$input, crs$target)], 
                       ntree=100,
                       mtry=15,
                       importance=TRUE,
                       na.action=na.roughfix,
                       replace=FALSE)

# Generate textual output of 'Random Forest' model.

crs$rf

# The `pROC' package implements various AUC functions.

require(pROC, quietly=TRUE)

# Calculate the Area Under the Curve (AUC).

roc(crs$rf$y, as.numeric(crs$rf$predicted))

# Calculate the AUC Confidence Interval.

ci.auc(crs$rf$y, as.numeric(crs$rf$predicted))

# List the importance of the variables.

rn <- round(importance(crs$rf), 2)
rn[order(rn[,3], decreasing=TRUE),]

# Time taken: 10.76 mins