rm(list=ls())
##---------------- ???????????? ----------------
#setwd(dir) #??????working directory???????????????
# MAC : setwd("/Users/rladiestaipei/R_DragonBall/") 
# Windows : setwd("C://Users/rladiestaipei/Desktop/R_DragonBall/")  
setwd("C:\\Users\\nxf51120\\Desktop\\RBasic\\2019DragonBall") #??????working directory???????????????

#????????????(??????????????????)
#install.packages(c("caTools", "caret", "dplyr", 'xgboost'),
#                 dependencies = TRUE)

#load packages
library(caTools)
library(caret)
library(dplyr)
library(xgboost)


### 3.2 ??????????????????????????????

dataset <- read.csv("train_new.csv")

# select features you want to put in models
# ?????????????????????????????????????????????????????????????????????(??????????????????)
dataset <- dataset %>% dplyr::select(SalePrice_log, X1stFlrSF, TotalBsmtSF, 
                                     YearBuilt, LotArea, Neighborhood, GarageCars, GarageArea, GrLivArea_stand, 
                                     MasVnrArea_stand, LotFronta?e_log, is_Fireplace, TotalBathrooms, TotalSF_stand)

# transfer all feature to numeric
cat_index <- which(sapply(dataset, class) == "factor")
dataset[cat_index] <- lapply(dataset[cat_index], as.numeric)

# Splitting the dataset into the Training set and Va?idation set
set.seed(1)
split <- sample.split(dataset$SalePrice_log, SplitRatio = 0.8)
training_set <- subset(dataset, split == TRUE)
val_set <- subset(dataset, split == FALSE)


# put testing & training data into two seperates Dmatrixs objects
tr_x <- as.?atrix(training_set)
tr_y <- training_set$SalePrice_log
val_x <- as.matrix(val_set)
val_y <- val_set$SalePrice_log

dtrain <- xgb.DMatrix(data = tr_x, label = tr_y) 
dval <- xgb.DMatrix(data = val_x, label = val_y)


### 3.3 ???XGBoost????????????????????????????????????(baseline)??????


#default parameters
default_params <- expand.grid(
  nrounds = 100,
  max_depth = 6,
  eta = 0.3,
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

train_control <- caret::trainControl(
  method = "none",
  verboseIter = FA?SE, # no training log
  allowParallel = TRUE # FALSE for reproducible results 
)

xgb_base <- caret::train(
  x = tr_x,
  y = tr_y,
  trControl = train_control,
  tuneGrid = default_params,
  method = "xgbTree",
  verbose = TRUE
)

xgb_base_rmse <- ModelMe?rics::rmse(val_y, predict(xgb_base, newdata = val_x))
xgb_base_rmse
# 0.004612619


### 3.4 XGBoost ???????????????


## Step 1. Number of iterations and learning rate

## ??????library(caret)?????????
# ???????????????grid search??????????????????
grid <- expand.grid(
  nrounds = seq(from = 200, to = 1000, by = 50),
  eta = c(0.025, 0.05, 0.1, 0.3),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0, 
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

control <- caret::trainControl(
  meth?d = "cv",
  number = 3, # cross validation with n(n=3) folds
  verboseIter = FALSE,
  allowParallel = FALSE # FALSE for reproducible results 
)

xgb_tune <- caret::train(
  x = tr_x,
  y = tr_y,
  trControl = control,
  tuneGrid = grid,
  method = "xgbTree?,
  verbose = TRUE
)

# ??????tune???????????????????????????
xgb_tune$bestTune


## Step 2. Maximum depth and minimum child weight

grid2 <- expand.grid(
  nrounds = seq(from = 200, to = 1000, by = 50),
  eta = 0.025, # ??????????????????????????? xgb_tune$bestTune$eta
  max_depth =  c(5, 6, 7), # ??????????????????????????? ifelse(xgb_tune$bestTune$max_depth == 2,
  # c(xgb_tune$bestTune$max_depth:4),
  # xgb_tune$bestTune$max_depth - 1:xgb_tune$bestTune$max_depth + 1),
  gamma = 0, 
  colsample_bytree = 1,
  min_child_weight = c(1, 2, 3),
  subsample = 1
)

xgb_tune2 <- car?t::train(
  x = tr_x,
  y = tr_y,
  trControl = control,
  tuneGrid = grid2,
  method = "xgbTree",
  verbose = TRUE
)
# ??????tune???????????????????????????
xgb_tune2$bestTune



## Step 3. Column and row sampling

grid3 <- expand.grid(
  nrounds = seq(from = 200, to = 1000, by = 50),
  eta = 0.025, # ??????????????????????????? xgb_tune$bestTune$eta
  max_depth =  6,  # ??????????????????????????? xgb_tune2$bestTune$max_depth
  gamma = 0, 
  colsample_bytree = c(0.4, 0.6, 0.8, 1.0),
  min_child_weight = 1, # ??????????????????????????? xgb_tune2$bestTune$min_child_weight
  subsample = c(0.5, 0.65, 0.8, 0.95, 1.0)
)

xgb_tune3 <- caret::train(
  x = tr_x,
  y = tr_y,
  trControl = control,
  tuneGrid = grid3,
  method = "xgbTree",
  verbose = TRUE
)
# ??????tune???????????????????????????
xgb_tune3$bestTune


## Step 4. Gamma

grid4 <- expand.grid(
  nrounds = seq(from = 200, to = 1000, by = 50),
  eta = 0.025, # ??????????????????????????? xgb_tune$bestTune$eta
  max_depth =  6,  # ??????????????????????????? xgb_tune2$bestTune$max_depth
  gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0),
  colsample_bytree = 1, # ??????????????????????????? xgb_tune3$bestTune$colsample_bytree
  min_child_weight = 1, # ??????????????????????????? xgb_tune2$bestTune$min_child_weight
  subsample = 0.5 # ??????????????????????????? xgb_tune3$bestTune$subsample
)

xgb_tune4 <- caret::train(
  x = tr_x,
  y = tr_y,
  trControl = control,
  tuneGrid = grid4,
  method = "xgbTree",
  verbose = TRUE
)
# ??????tune???????????????????????????
xgb_tune4$bestTune



## Step 5. Reducing the learning rate and increase nrounds

grid5 <- expand.grid(
  nrounds = seq(from = 500, to = 10000, by = 100),
  eta = c(0.01, 0.015, 0.025, 0.05, 0.1), 
  max_depth =  6,  # ??????????????????????????? xgb_tune2$bestTune$max_depth
  gamma = 0, # ??????????????????????????? xgb_tune4$bestTune$gamma
  colsample_bytree = 1, # ??????????????????????????? xgb_tune3$bestTune$colsample_bytree
  min_child_weight = 1, # ??????????????????????????? xgb_tune2$bestTune$min_child_weight
  subsample = 0.5 # ??????????????????????????? xgb_tune3$bestTune$subsample
)

xgb_tune5 <- caret::train(
  x = tr_x,
  y = tr_y,
  trControl = control,
  tuneGrid = grid5,
  method = "xgbTree",
  verbose = TRUE
)
# ??????tune???????????????????????????
xgb_tune5$bestTune


## Step 6. Fit model with final parameters

## ?????????????????????
final_grid <- expand.grid(
  nrounds = xgb_tune5$bestTune$nrounds,
  eta = xgb_tune5$bestTune$eta,
  max_depth = xgb_tune5$bestTune$max_depth,
  gamma = xgb_tune5$bestTune$gamma,
  colsample_bytree = xgb_tune5$bestTune$colsample_bytree,
  min_child_weight?= xgb_tune5$bestTune$min_child_weight,
  subsample = xgb_tune5$bestTune$subsample
)
final_grid

# ???tune?????????????????????????????????
xgb_model <- caret::train(
  x = tr_x,
  y = tr_y,
  trControl = train_control,
  tuneGrid = final_grid,
  method = "xgbTree",
  verbose = TRUE
)


## Step 7. Compare perfomrance on validation set with baseline model

# ???????????????????????????????????????????????????
xgb_tuned_rmse <- ModelMetrics::rmse(val_y, predict(xgb_model, newdata = val_x))

xgb_tuned_rmse # tune??????????????????????????????RMSE
xgb_base_rmse # ??????????????????????????????????????????RMSE

# RMSE ????????????
xgb_base_rmse - xgb_tuned_rmse
## ???baseline?????????????????????,??????????????? RMSE ??? 0.0046 ???????????? 0.0011!!!


## ???????????????

## ??????????????????????????????????????? "Bias-Variance Tradeoff"????????????
# Dana: ???????????????????????????????????????,??????????????????????????????????????????,???????????????????????????????????????????????????,
#??????????????????????????????,??????minimal variance prior to smaller bias??????????????????????????????.
