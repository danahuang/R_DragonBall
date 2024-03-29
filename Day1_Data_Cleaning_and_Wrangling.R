rm(list=ls())
##---------------- ???????????? ----------------
setwd("C:\\Users\\nxf51120\\Desktop\\RBasic\\2019DragonBall") #??????working directory???????????????
# setwd("/Users/hsinyuchan/R_DragonBall/") 

#????????????(??????????????????)
# install.packages(c("tidyverse", "ploty", "zoo", "lubridate", "rmarkdown","data.table", "DT", "kableExtra"), dependencies = TRUE)

#load packages
library(DT)
library(zoo)
library(plotly)
library(lubridate)
library(rmarkdown)
library(data.table)
library(?idyverse)
library(kableExtra)
options(dplyr.print_max=1e9)  ##dana???

##------------------------------------------------
## Part 1: Read and load data
##------------------------------------------------
#??????????????????workspace???,?????????????????????????????????
train1<-read.csv("train.csv")  
train2<-fread("train.csv")
#str(train1)        #??????????????????
str(train1[,1:10])  #??????????????????????????????,????????????????????????
#str(train2)
str(train2[,1:10])


##------------------------------------------------
## Part 2:??????data??????
##------------------------------------------------
#??????raw data 
train0<-read.csv("train.csv", stringsAsFactors = FALSE)
test0<-read.csv("test.csv", stringsAsFactors = FALSE)
dim(train0)  #training data??????1460??????????????????,81?????????
dim(test0)   #testing data??????1460??????????????????,80?????????
#training ??? testing ???????????????training data???????????? (SalesPrice),??????testing data?????????????????????????????????????????????
colnames(train0)[!colnames(train0) %in% colnames(test0)] 


##------------------------------------------------
## Part 3: ???????????????????????????
##------------------------------------------------
dat<-train0%>%select(10:15)
head(dat)

dat<-train0%>%select(MSZoning, Utilities, HouseStyle, Heating, YearBuilt, SalePrice)
head(dat)

#1. ?????????????????????:????????????????????????Lot,Bsmt??????,?????????Condition??????
dat<-train0%>%select(contains("Lot"), starts_with("Bsmt"), ends_with("Condition"))
head(dat)

#2. ??????????????????pattern
dat<-train0%>%select(matches("Yr|Year|year|yr"))  #????????????????????????
head(dat)

#3. ????????????
dat<-train0%>%select(-PoolArea, -Fence, -matches("Bsmt|Lot|Garage"))  #PoolArea, Fence, ??????Bsmt???Lot???Garage??????????????????
colnames(dat)


plot_ly(train0, x=~SalePrice, type="histogram")

range(train0$SalePrice)  #???????????????????????????
dat<-train0%>%filter(SalePrice>=100000, SalePrice<=300000)
range(dat$SalePrice)  #????????????????????????
#??????100000~300000???????????????????????????84.3%
scales::percent(nrow(dat)/nrow(train0))

table(train0$SaleType)  #SaleType??????WD????????????
#???SaleTeyp=="WD"?????????(WD:Warranty Deed - Conventional)
dat<-train0%>%filter(SaleType=="WD") 
table(dat$SaleType)

table(train0$YrSold, train0$SaleType)  #???????????????SaleType?????????
#???2008???????????????,??????SaleType???New(??????????????????)?????????
dat<-train0%>%        
  filter(YrSold<2008, SaleType=="New")   
table(dat$YrSold, dat$SaleType)

dat<-train0%>%slice(1000:1001)  #???1000-1001?????????
head(dat[,1:3])

#?????????????????????
dat<-train0%>%top_n(5, SalePrice)    
#??????Id, ??????????????????????????????????????????
head(dat%>%select(Id, Neighborhood, SalePrice)%>%arrange(-SalePrice))

dat<-train0%>%
  group_by(Neighborhood)%>%
  summarise(low=min(SalePrice),
            high=max(SalePrice),
            average=mean(SalePrice),
            sd=sd(SalePrice))%>%
  arra?ge(-average)

#show table
kable(dat)%>%
  kable_styling("striped", font_size=12, full_width =F, position = "center") %>%
  column_spec(c(1), bold=T)

dat<-train0%>%
  group_by(Neighborhood, Street)%>%
  summarise(low=min(SalePrice),
            high=max(Sa?ePrice),
            average=mean(SalePrice),
            sd=sd(SalePrice))

#show table
kable(dat)%>%
  kable_styling("striped", font_size=12, full_width =F, position = "center") %>%
  column_spec(c(1), bold=T)

dat<-train0%>%
  group_by(YearBuilt, MasVnr?ype)%>%
  summarise(average=mean(SalePrice))%>%
  arrange(-average)

plot_ly(dat, x = ~YearBuilt, y = ~average, text = ~MasVnrType, type = 'scatter', mode = 'markers', size = ~average, color = ~MasVnrType,
        #Choosing the range of the bubbles' sizes:?        sizes = c(10, 80),
        marker = list(opacity = 0.5, sizemode = 'diameter')) %>%
  layout(title = 'Estate Sale Price by Neighborhood',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE),
         showlegend = TRUE)
?dat<-train0%>%   
  select(BsmtQual)%>%
  rename(BsmtHght=BsmtQual)   
head(dat)

colnames(train0%>%select(contains("Bsmt"))) 

#???BsmtFullBath, BsmtHalfBath??????,??????????????????????????????BsmtBath
Bsmt<-train0%>%                    
  select(matches("Bsmt.*Bath"))%>%
  mutate(BsmtBath=case_when(BsmtFullBath>0|BsmtHalfBath>0~1,  #?????????Bath,????????????????????????1,?????????0
                            TRUE~0))
head(Bsmt%>%arrange(-BsmtBath))

dat<-train0%>%                         
  group_by(OverallQual)%>%
  mutate(average_SalePrice=mean(SalePrice))%>%    
  select(Id, OverallQual, SalePrice)
dat[1:10,] 

dcr0<-read.delim?"data_description.txt", header = FALSE, stringsAsFactors = FALSE)
datatable(dcr0)

dcr<-dcr0%>%
  #????????????????????????????????????????????????(by tab or space),????????????????????????
  mutate(feature=sapply(strsplit(V1, '\t|[[:space:]]'), "[", 1))%>%  
  filter(!is.na(feature))%>%           #??????????????????fill, ????????????????????????NA??????
  mutate_all(na_if, "")%>%             #???feature???????????????NA???????????????NA????????????
  fill(feature, .direction="down")%>% 
  rename(value=V1, description=V2)%>%  #???????????????????????????
  select(feature, value, description)

datatable(dcr, options = list(pageLength=20))


##------------------------------------------------
## Part 4: ???????????????????????????
##------------------------------------------------
#BsmtFinType1
fintype1<-train0%>%             #????????????Id?????????BsmtFinType1???????????????
  group_by(Id, BsmtFinType1)%>%
  summarise(count=n())%>%
  spread(BsmtFinType1, count, fill=0)  #fill=0???????????????Id????????????Type???,???0??????,????????????NA
head(fintype1)

#BsmtFinType2
fintype2<-train0%>%
  group_by(Id, BsmtFinType2)%>%
  summarise(count=n())%>%
  spread(BsmtFinType2, count, fill=0)
head(fintype2)

#??????????????????
bsmtfintype<-
  #bind_rows???rbind?????????????????????????????????????????????????????????,?????????????????????????????????
  bind_rows(fintype1, fintype2) %>%            
  group_by(Id) %>%
  summarise_all(funs(sum(., na.rm = TRUE)))%>% 
  rename_all(function(x) paste0("BsmtFinType_", x)) #??????"BsmtFinType"??????prefix

#??????train data
train<-train0%>%
  left_join(bsmtfintype, by=c("Id"="BsmtFinType_Id"))

datatable(train%>%select(Id, contains("BsmtFinType")))

##------------------------------------------------
## ???????????????
##------------------------------------------------
#?????????training data???1-3???????????????????????????,?????????????????????????????????,????????????,??????,?????????(????????????)???

# 1. Area sumarea=0.8*LotArea+0.2*(MasVnrArea+GrLivArea+GarageArea+PoolArea
dat<-train0%>%select(Id,contains("area"))
head(dat) #LotArea MasVnrArea GrLivArea GarageArea PoolArea
dim(dat)
Areasum<-dat%>%
  group_by(Id)%>%
  summarise(sumarea=0.8*LotArea+0?2*(MasVnrArea+GrLivArea+GarageArea+PoolArea))
head(Areasum)
dim(Areasum)
train<-train0%>%
  left_join(Areasum, by=c("Id"))

# 2. Location

dat<-train0%>%
  group_by(Neighborhood)%>%   #Remove  Street since I think it's not an evident variable. 
  summarise?low=min(SalePrice),
            high=max(SalePrice),
            average=mean(SalePrice),
            sd=sd(SalePrice))  #order 
head(dat) 

# 3. Year
dat<-train0%>%select(SalePrice,matches("Yr|Year|year|yr"))  #????????????????????????
head(dat)  #Year built influences SalesPrice the most. But all influences a bit except YrSold.

