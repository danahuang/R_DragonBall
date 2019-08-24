rm(list=ls())
##---------------- ???????????? ----------------
##---------------- ???????????? ----------------
#setwd(dir) #??????working directory???????????????
# MAC : setwd("/Users/rladiestaipei/R_DragonBall/") 
setwd("C:\\Users\\nxf51120\\Desktop\\RBasic\\2019DragonBall") #??????working directory???????????????
# Windows : setwd("C://Users/rladiestaipei/Desktop/R_DragonBall/")  

#????????????(??????????????????)
#install.packages(c("caTools", "caret", "dplyr", 'xgboost',"Metrics"),dependencies = TRUE)

#load packages
library(caTools)
library(caret)
library(dplyr)
library(xgboost)
library(Metrics)

##------------------------------------------------
## Part 2 : Mo?el Evaluation in R
##------------------------------------------------
##??????????????????????????????
dataset <- read.csv("train_new2.csv")

##???????????????, ?????????Day 4?????????Linear Regression & XGBoost ???????????????
# select features you want to put in models
# ?????????????????????????????????????????????????????????????????????(??????????????????)
dataset <- dataset %>% dplyr::select(SalePrice_log, X1stFlrSF, TotalBsmtSF, 
                                     YearBuilt, LotArea, Neighborhood, GarageCars, GarageArea, GrLivArea_stand, 
                                     MasVnrArea_stand, LotFronta?e_log, is_Fireplace, TotalBathrooms, TotalSF_stand)

# Splitting the dataset into the Training set and Validation set
# library(caTools)
set.seed(1)
split <- sample.split(dataset$SalePrice_log, SplitRatio = 0.8)
training_set <- subset(dataset, split == TRU?)
val_set <- subset(dataset, split == FALSE)

##Linear Regression
# Fitting Multiple Linear Regression to the Training set
Reg <- lm(formula = SalePrice_log ~ ., data = training_set)


##XGBoost
# library(xgboost)
# ???XGBoost??????matrix?????????,??????????????????????????????????????????,???????????????

# transfer all feature to numeric
training_set_new <- training_set %>% dplyr::select(-SalePrice_log)
val_set_new <- val_set %>% dplyr::select(-SalePrice_log)
cat_index <- which(sapply(training_set_new, class) == "factor")
training_set_new[cat_index] <- l?pply(training_set_new[cat_index], as.numeric)
val_set_new[cat_index] <- lapply(val_set_new[cat_index], as.numeric)

# put testing & training data into two seperates Dmatrixs objects
labels <- training_set$SalePrice_log
dtrain <- xgb.DMatrix(data = as.matri?(training_set_new),label = labels) 
dval <- xgb.DMatrix(data = as.matrix(val_set_new))

# set parameters
param <-list(objective = "reg:linear",
             booster = "gbtree",
             eta = 0.01, #default = 0.3
             gamma=0,
             max_?epth=3, #default=6
             min_child_weight=4, #default=1
             subsample=1,
             colsample_bytree=1
)

# Fitting XGBoost to the Training set
set.seed(1)
xgb_base <- xgb.train(params = param, data = dtrain, nrounds = 3000
              ?       #watchlist = list(train = dtrain, val = dval),
                      #print_every_n = 50, early_stopping_rounds = 300
)


## Performance Evaluation

# RMSE
# Day4 & Day5????????????2???????????????????????????RMSE (Day4???Metrics & Day5??????caret??????ModelMetrics)

# ????????????validation data?????????????????????, ??????Day4???Metrics????????????Linear Regression & XGBoost ????????????RMSE
# 1. Predicting the Validation set results
# Linear Regression
pred_reg <- predict(Reg, newdata = val_set)
# XGBoost
pred_xgb_base <- predict(xgb_base, dval)

# 2. RMSE-Metrics Package
# Linear Regression
rmse(val_set$SalePrice_log, pred_reg)
# XGBoos?
rmse(val_set$SalePrice_log, pred_xgb_base)

# ????????????, ??????MSE & RMSE, ???XGBoost??????????????????
mse_cal <- mean((pred_xgb_base - val_set$SalePrice_log)**2)
print(mse_cal)

rmse_cal <- sqrt(mse_cal)
print(rmse_cal)

# 3. ??????MAE
# XGBoost - Metrics Package
mae(val_set$SalePrice_log, pred_xgb_base)

# ??????????????????, 0.1442952 = 0.1442952

##------------------------------------------------
## Part 3 : ???????????????????????? Kaggle ?????????
##------------------------------------------------
# ??????Kaggle??????????????????????????????testset log(SalePrice)????????????????????????RMSE,????????????
# ??????Validation set RMSE???????????????
# Steps:
# 1. ????????????????????????testset
# 2. ?????????????????????Sale_Price??????log??????,????????????????????????exp??????????????????Sale Price

# ??????log?????????
x <- 87
x_log <- log(x)
exp(x_log)

##------------------------------------------------
## ???????????????
##------------------------------------------------
#A. ????????????????????????,????????????testset???????????????Kaggle????????????! ????????????Kaggle????????????????????????RMSE?????????
#B. ?????????????????????R-Squared

#import testing data
test_set <- read.csv("test_new2.csv")

test_set <- test_set %>% dplyr::select( X1stFlrSF, TotalBsmtSF, 
                                        YearBuilt, LotArea, Neighborhood, GarageCars, GarageArea, GrLivArea_stand, 
    ?                                   MasVnrArea_stand, LotFrontage_log, is_Fireplace, TotalBathrooms, TotalSF_stand)


# change value to numeric type
cat_index <- which(sapply(test_set, class) == "factor")
test_set[cat_index] <- lapply(test_set[cat_index], a?.numeric)
dtest <- xgb.DMatrix(data = as.matrix(test_set))

pred_xgb_base <- predict(xgb_base, dtest)
pred_xgb_base_f<-exp(pred_xgb_base)

test_set_f <- read.csv("test_new2.csv")
test_set_f$SalesPrice<-pred_xgb_base_f
pred_xgb_base_ff<-test_set_f %>% dplyr?:select(Id,SalesPrice)

write.csv(pred_xgb_base_ff,"sample_submission.csv",row.names=FALSE)
