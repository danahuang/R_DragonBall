rm(list=ls())
##---------------- ???????????? ----------------
#setwd(dir) #??????working directory???????????????
setwd("C:\\Users\\nxf51120\\Desktop\\RBasic\\2019DragonBall") 
# MAC : setwd("/Users/rladiestaipei/R_DragonBall/") 
# Windows : setwd("C://Users/rladiestaipei/Desktop/R_DragonBall/")  

#????????????(??????????????????)
# install.packages(c("tidyverse", "ploty", "zoo", "lubridate", "rmarkdown","data.table", "DT", "kableExtra"), dependencies = TRUE)

#load packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(magrittr)
library(corrplot)
library(gridExtra)
l?brary(plotly)
options(dplyr.print_max=1e9)

##------------------------------------------------
## Part 1 : load data and split
##------------------------------------------------
# ????????????
train0 <- read.csv("train.csv", stringsAsFactors = FALSE)
test0 <- read.csv("test.csv", stringsAsFactors = FALSE)

# ?????? numeric and character ??????
num_features <- names(which(sapply(train0, is.numeric)))
cat_features <- names(which(sapply(train0, is.character)))
train_numeric <- train0[, names(train0) %in% num_features]
train_categoric <- train0[, names(train0) %in% cat_features]

print(num_features?
print(cat_features)


##------------------------------------------------
## Part 2 : ?????? missing value
##------------------------------------------------
# ??????num_features???36?????????
# MSSubClass(????????????)???OverallQual(????????????????????????)???OverallCond(??????????????????) ???????????????factor,????????????
train0$OverallCond <- as.factor(train0$OverallCond)
train0$OverallQual <- as.factor(train0$OverallQual)
train0$MSSubClass <- as.factor(train0$MSSubClass)

# ???????????????????????????????????????
missing_values <- sapply(train0, function(x) sum(is.na(x)))
null_count <- data.frame(Count = missing_values, Proportion = missing_values/nrow(train0))
null_count_gteZero <- null_count[null_count$Count > 0, ]
null_count_gteZero[order(-null_count_gteZero$Co?nt),]

# ??????????????????NA?????????
train_non_null <- train0 %>% 
  select(-c(rownames(null_count_gteZero), OverallCond, OverallQual, MSSubClass))


##------------------------------------------------
## Part 3 : Various plots with SalePrice
##------------------------------------------------?# ???????????????NA?????????,????????????????????????
match_num_features <- paste(num_features, collapse = "|")
train_non_null_df <- select(train_non_null, matches(match_num_features))

theme_set(theme_bw())  # pre-set the bw theme.
# ??????SF(??????)????????? 
train_SF <- select(train_non_null, matches("SF|SalePrice"))
# ???????????????SalePrice?????????
train_SF %>%
  # keep(is.numeric) %>% 
  gather(-SalePrice, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = SalePrice)) +   # Plot the values
  facet_wrap(~ var, scales = "free") +   # In separate panels
  geom_point() 

# ??????SF(??????)????????? 
train_Time <- select(train_non_null, matches("Yr|Year|Mo|year|yr|SalePrice"))
train_Time %>%
  gather(-SalePrice, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = SalePrice)) +   # Plot the values
  facet_wrap(~ var, scales = "free") +   # I? separate panels
  geom_point() 

# ??????Area(????????????)????????? 
train_Area <- select(train_non_null, matches("Area|SalePrice"))
train_Area %>%
  gather(-SalePrice, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = SalePrice)) +   # Plot the values
  facet_wrap(~ var, scales = "free") +   # In separate pan?ls
  geom_point() 

# ???????????????NA?????????,????????????????????????
match_cat_features <- paste(cat_features, collapse = "|")
train_non_null_df <- select(train_non_null, matches(match_num_features))

# ??????????????????????????? 
train_outside <- select(train_non_null, matches("Roof|MSSubClass|LotShape|Exterior|SalePrice"))
# ?????????????????????SalePrice?????????
train_outside %>%
  gather(-SalePrice, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = SalePrice)) +   # Plot the values
  facet_wrap(~ var, scales = "free") +   # In separate panels
  geom_boxplot(na.rm = T) +
  theme(axis.text.x = element_?ext(angle = 90, hjust = 1))

# ??????????????????????????? 
train_inside <- select(train_non_null, matches("BldgType|Utilities|House|Bsmt|TotRmsAbvGrd|Fireplace|SalePrice"))
train_inside %>%
  gather(-SalePrice, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = SalePrice)) +   # Plot the values
  face?_wrap(~ var, scales = "free") +   # In separate panels
  geom_boxplot(na.rm = T) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# ??????Area(????????????)????????? 
train_other <- select(train_non_null, matches("Electrical|Neighborhood|Street|Garage|MSZoning|SalePrice"))
train_other %>%
  gather(-SalePrice, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = SalePrice)) +   # Plot the values
  facet_wrap(~?var, scales = "free") +   # In separate panels
  geom_boxplot(na.rm = T) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

correlations <- cor(train_non_null_df, use = "complete.obs")
cor_bar <- data.frame("cor" = correlations[,"SalePrice"])
co?_bar$item <- row.names(cor_bar)
cor_bar <- cor_bar[order(-cor_bar$cor),][-1,]
cor_bar$item <- factor(cor_bar$item, levels=cor_bar$item)
ggplot(cor_bar) + 
  geom_bar(stat='identity', aes(x = item, y = cor), width=.5)  +
  labs(title= "Correlations Bars") +?
  coord_flip()


##------------------------------------------------
## Part 5 : ?????? SalePrice ??????
##------------------------------------------------
ggplot(data = train0[!is.na(train0$SalePrice),], aes(x=SalePrice)) +
  geom_histogram(fill="blue", binwidth = 10000) +
  scale_x_continuous(breaks= seq(0, 800000, by=100000))
summary(train0[!is.na(train0$?alePrice),]$SalePrice)

train0$SalePrice <- log(train0$SalePrice)
ggplot(train0, aes(x=SalePrice)) + 
  geom_histogram(fill="blue", binwidth = .05)
summary(train0[!is.na(train0$SalePrice),]$SalePrice)


##------------------------------------------------
##????????????????
##------------------------------------------------
#????????? training data ?????????????????????????????????,???????????????????????????
# 1. Area
#Create the variable
dat<-train0%>%select(Id,contains("area"))
head(dat) #LotArea MasVnrArea GrLivArea GarageArea PoolArea
Areasum<-dat%>%
  group_by(Id)%>%
  summarise(sumarea=0.8*LotArea+0.2*(MasVnrArea+GrLivArea+GarageArea+PoolArea))
head(Are?sum)
train<-train0%>%
  left_join(Areasum, by=c("Id"))
head(train)

#Plot the variable
theme_set(theme_bw())  # pre-set the bw theme.

Areasum_train<- train%>%  select (sumarea, SalePrice,)
Areasum_train %>%
  gather(-SalePrice, key = "var", value = "value?) %>%
  ggplot(aes(x = value, y = SalePrice)) +   # Plot the values
  facet_wrap(~ var, scales = "free") +   # In separate panels
  geom_point() 





