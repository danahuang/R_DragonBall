rm(list=ls())
##---------------- ???????????? ----------------
#setwd(dir) #??????working directory???????????????
# MAC : setwd("/Users/rladiestaipei/R_DragonBall/") 
setwd("C:\\Users\\nxf51120\\Desktop\\RBasic\\2019DragonBall") 
# Windows : setwd("C://Users/rladiestaipei/Desktop/R_DragonBall/")  

#--- Check Package (??????????????????,?????????????????????)
packages.need <- c("caTools", "e1071", 'rpart', 'randomForest', 'xgboost', 'dplyr', 'Metrics')

packages.download <- !packages.need %in% installed.packages()[,"Package"]

if(any(packages.download))
  install.packages(packages.need[packages.download],depe?dencies=TRUE)

#--- Load packaes
lapply(packages.need, require, character.only = TRUE)


##---------------- Load data and split ----------------
#--- Load data (?????? Day3 Feature Engineering ??????????????????????????????,??????????????????)
dataset <- read.csv('train_new.csv', stringsAsFactors = T)

# select features you want to put in models
# ?????????????????????????????????????????????????????????????????????(??????????????????)
dataset <- dataset %>% dplyr::select(SalePrice_log, X1stFlrSF, TotalBsmtSF, YearBuilt, LotArea,
                                     Neighborhood, GarageCars, GarageArea,
                                     GrLivArea_stand, MasVnrArea_stand, LotFrontage?log,
                                     is_Fireplace, TotalBathrooms, TotalSF_stand)

# ?????????????????????csv???????????????numeric,???????????????factor
dataset$is_Fireplace <- as.factor(dataset$is_Fireplace)

# Splitting the dataset into the Training set and Validation set
# library(caTools)
set.seed(1)
split <- sample.split(dataset$SalePrice_log, SplitRatio = 0.8)
training_set <- subset(dataset, s?lit == TRUE)
val_set <- subset(dataset, split == FALSE)


##------------------------------------------------
## Part 1 : linear regression
##------------------------------------------------
# Fitting Multiple Linear Regression to the Training set
regressor?<- lm(formula = SalePrice_log ~ .,
                data = training_set)

# Predicting the Test set results
y_pred <- predict(regressor, newdata = val_set)

# performance evaluation
rmse(val_set$SalePrice_log, y_pred)


##-----------------------------------?------------
## Part 2 : SVR
##------------------------------------------------
# Fitting SVR to the dataset
# library(e1071)
regressor <- svm(formula = SalePrice_log ~ .,
                 data = training_set,
                 type = 'eps-regression',
    ?            kernel = 'radial')

# Predicting the Test set results
y_pred <- predict(regressor, newdata = val_set)

# performance evaluation
rmse(val_set$SalePrice_log, y_pred)


##------------------------------------------------
## Part 3 : Decision Tree
#?------------------------------------------------
# Fitting Decision Tree Regression to the dataset
# library(rpart)
regressor <- rpart(formula = SalePrice_log ~ .,
                   data = training_set,
                   control = rpart.control(minsplit ? 1))

# Predicting the Test set results
y_pred <- predict(regressor, newdata = val_set)

# performance evaluation
rmse(val_set$SalePrice_log, y_pred)


##------------------------------------------------
## Part 4 : Random Forest
##-------------------------?----------------------
# Fitting Random Forest Regression to the dataset
# library(randomForest)
set.seed(1)
regressor <- randomForest(formula = SalePrice_log ~ .,
                          data = training_set,
                          ntree = 500)

# Pre?icting the Test set results
y_pred <- predict(regressor, newdata = val_set)

# performance evaluation
rmse(val_set$SalePrice_log, y_pred)


##------------------------------------------------
## Part 5 : XGBoost
##-------------------------------------------?----
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
regressor <- xgb.train(params = param, data = dtrain, nrounds = 3000
             ?         #watchlist = list(train = dtrain, val = dval),
                       #print_every_n = 50, early_stopping_rounds = 300
)

# Predicting the Test set results
y_pred <- predict(regressor, dval)

# performance evaluation
rmse(val_set$SalePrice_log, y_?red)



##------------------------------------------------
## ???????????????
##------------------------------------------------
#???????????????randomforest???XGBoost???????????????,????????????????????????????????????rmse

##------------------------------------------------
## Part 4 : Random Forest
##------------------------------------------------
# Fitting Random Forest Regression to the dataset
# library(randomForest)
set.seed(1)
regressor <- randomForest(formula = ?alePrice_log ~ .,
                          data = training_set,
                          ntree = 50)   #dana: change to 50 

# Predicting the Test set results
y_pred <- predict(regressor, newdata = val_set)

# performance evaluation
rmse(val_set$SalePric?_log, y_pred)

##------------------------------------------------
## Part 5 : XGBoost
##------------------------------------------------
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
             max_?epth=2, #default=6   dana chaged from 3
             min_child_weight=5, #default=1  dana chaged from 4
             subsample=1,
             colsample_bytree=1
)

# Fitting XGBoost to the Training set
set.seed(1)
regressor <- xgb.train(params = param, da?a = dtrain, nrounds = 3000
                       #watchlist = list(train = dtrain, val = dval),
                       #print_every_n = 50, early_stopping_rounds = 300
)

# Predicting the Test set results
y_pred <- predict(regressor, dval)

# performance ?valuation
rmse(val_set$SalePrice_log, y_pred)
