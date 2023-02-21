

# 
# boursenews<-readRDS("C:\\Users\\98939\\Desktop\\data_twitter_60\\_boursenews_ir (2).RData")
# Deghtesad<-readRDS("C:\\Users\\98939\\Desktop\\data_twitter_60\\_deghtesad.RData")
# FarsNews_Agency<-readRDS("C:\\Users\\98939\\Desktop\\data_twitter_60\\_FarsNews_Agency.RData")
# Navad_eghtesadi<-readRDS("C:\\Users\\98939\\Desktop\\data_twitter_60\\_NavadeEghtesadi.RData")
# SharghDaily<-readRDS("C:\\Users\\98939\\Desktop\\data_twitter_60\\_SharghDaily.RData")
# TasnimNews<-readRDS("C:\\Users\\98939\\Desktop\\data_twitter_60\\_Tasnimnews_Fa (1).RData")
# TejaratNews<-readRDS("C:\\Users\\98939\\Desktop\\data_twitter_60\\_tejaratnews.RData")
# 


boursenews<-readRDS("C:\\Users\\98939\\Desktop\\data_twitter_new\\_boursenews_ir (2).RData")
Deghtesad<-readRDS("C:\\Users\\98939\\Desktop\\data_twitter_new\\_deghtesad.RData")
FarsNews_Agency<-readRDS("C:\\Users\\98939\\Desktop\\data_twitter_new\\_FarsNews_Agency.RData")
Navad_eghtesadi<-readRDS("C:\\Users\\98939\\Desktop\\data_twitter_new\\_NavadeEghtesadi.RData")
SharghDaily<-readRDS("C:\\Users\\98939\\Desktop\\data_twitter_new\\_SharghDaily.RData")
TasnimNews<-readRDS("C:\\Users\\98939\\Desktop\\data_twitter_new\\_Tasnimnews_Fa (1).RData")
TejaratNews<-readRDS("C:\\Users\\98939\\Desktop\\data_twitter_new\\_tejaratnews.RData")




colnames(boursenews)<-c("Date","sentiment_BourseNews")
colnames(Deghtesad)<-c("Date","sentiment_donyaie_eghtesad")
colnames(FarsNews_Agency)<-c("Date","sentiment_FarsNews")
colnames(Navad_eghtesadi)<-c("Date","sentiment_navad_eghtesadi")
colnames(SharghDaily)<-c("Date","sentiment_Shargh")
colnames(TasnimNews)<-c("Date","sentiment_TasnimNews")
colnames(TejaratNews)<-c("Date","sentiment_TejaratNews")

# 
# boursenews$sentiment_BourseNews<-ifelse(boursenews$sentiment_BourseNews>=0.5,ifelse(boursenews$sentiment_BourseNews==0.5,0,1),-1)
# Deghtesad$sentiment_donyaie_eghtesad<-ifelse(Deghtesad$sentiment_donyaie_eghtesad>=0.5,ifelse(Deghtesad$sentiment_donyaie_eghtesad==0.5,0,1),-1)
# FarsNews_Agency$sentiment_FarsNews<-ifelse(FarsNews_Agency$sentiment_FarsNews>=0.5,ifelse(FarsNews_Agency$sentiment_FarsNews==0.5,0,1),-1)
# Navad_eghtesadi$sentiment_navad_eghtesadi<-ifelse(Navad_eghtesadi$sentiment_navad_eghtesadi>=0.5,ifelse(Navad_eghtesadi$sentiment_navad_eghtesadi==0.5,0,1),-1)
# SharghDaily$sentiment_Shargh<-ifelse(SharghDaily$sentiment_Shargh>=0.5,ifelse(SharghDaily$sentiment_Shargh==0.5,0,1),-1)
# TejaratNews$sentiment_TejaratNews<-ifelse(TejaratNews$sentiment_TejaratNews>=0.5,ifelse(TejaratNews$sentiment_TejaratNews==0.5,0,1),-1)
# TasnimNews$sentiment_TasnimNews<-ifelse(TasnimNews$sentiment_TasnimNews>=0.5,ifelse(TasnimNews$sentiment_TasnimNews==0.5,0,1),-1)
# 

library(lubridate)
library(dplyr)
library(ggplot2)
library(forecast)
library(tidyverse, quietly = TRUE)
library(bsts, quietly = TRUE) 
library(DescTools)
library(quantmod)




target="adjClose"



#Normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

# standardize<- function(data, na.rm = TRUE) {
#   return((data- mean(data)) /(sd(data)))
# }





#A<-c("open","high","low","Close", "volume", "count","adjClose")

#A<-c("Close")  

data_asli<-read.csv("C:\\Users\\98939\\Documents\\TseClient 2.0\\Adjusted\\XTPI6.csv",encoding="UTF-8")
namad<-data_asli[1,1]
data_asli<-data_asli[,-1]
data_asli[,1] <- as.Date(as.character(data_asli[,1]), "%Y%m%d")
colnames(data_asli)[1]<-"Date"
data_asli<-data_asli[,c(1,10)]

#data_asli$adjClose<-log(data_asli$adjClose)


library(dplyr)
# new<-merge(x=Bourse24, y=trend_data, by= 'Date')

trend_data<-readRDS("C:\\Users\\98939\\Desktop\\trend_data.RData")

#put all data frames into list
twitter_list <- list(data_asli,boursenews,Deghtesad,SharghDaily,FarsNews_Agency,Navad_eghtesadi,TasnimNews,TejaratNews,trend_data)

#merge all data frames in list
data<-Reduce(function(x, y) merge(x, y, by= 'Date'), twitter_list)

#data<-data%>%filter(Date>="2021-09-28")

#data<-merge(x=data_asli[,c(1,10)], y=new, by= 'Date')
#data<-data%>%filter(Date<="2022-09-20")


#data[,(3:ncol(data))]<-apply(data[,(3:ncol(data))],2,standardize)
data[,(3:ncol(data))]<-as.data.frame(lapply(data[,3:ncol(data)], normalize))



#data_asli<-data_asli[1:(nrow(data_asli)-2),]
#data_asli<-data_asli[(nrow(data_asli)-3000):(nrow(data_asli)),]

number_test=round(0.15*nrow(data))



num_prediction<-number_test+1

data_train<-data[-c((nrow(data)-number_test):nrow(data)),]
data_test<-data[c((nrow(data)-number_test):nrow(data)),]


colnames(data_train)[1]<-"date"
data_train <- as.xts(data_train[,c(2:ncol(data_train))],order.by = data_train$date)
str(data_train)
dim(data_train)

#data_train<-data_train[, !duplicated(colnames(data_train))]
data_train<-data_train[,-c(103,216)]
data_train<-data_train[,-c(109)]
data_train<-data_train[,-c(215)]
#which(colnames(data_train)=='ÃÂªÃÂÃÂ§ÃÂÃÂ ÃÂÃÂ³ÃÂªÃÂ ÃÂ§ÃÂ')


colnames(data_test)[1]<-"date"
data_test <- as.xts(data_test[,c(2:ncol(data_test))],order.by = data_test$date)
dim(data_test)

#data_train<-data_train[, !duplicated(colnames(data_train))]
data_test<-data_test[,-c(103,216)]
data_test<-data_test[,-c(109)]
data_test<-data_test[,-c(215)]


ncol(data_train)


Sys.setlocale(locale = "persian")



#############################################################
#############BST_LSTM by regrssor
#############################################################
MAPE11<-c()
for(i in c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)){
prior <-SpikeSlabPrior(x=model.matrix(adjClose~., data=as.ts(data_train) ),
                       y=as.ts(data_train$adjClose),
                       expected.r2 = 0.6,
                       expected.model.size =10,  # expect 3 nonzero predictors
                       #prior.df = .01,           # weaker prior than the default
                       prior.inclusion.probabilities=rep(i,ncol(data_train)),
                       diagonal.shrinkage = 0# use Zellner's prior
)


ss<-list()
#ss <-AddSemilocalLinearTrend(ss,as.ts(train_data))
#ss <-AddLocalLevel(ss,as.ts(data_train$adjClose))
#ss <- AddMonthlyAnnualCycle(ss,data_train)
ss <-AddLocalLinearTrend(ss,as.ts(data_train$adjClose) )
ss <- AddSeasonal(ss, as.ts(data_train$adjClose), nseasons =52,season.duration=7)

bsts.reg.priors <- bsts(adjClose~., state.specification = ss,data=as.ts(data_train),niter = 10000,prior=prior, seed=123)
burn <-9000


bsts_res<- as.numeric(residuals(bsts.reg.priors,burn=burn,mean.only=T))

#bsts_res<-colMeans(bsts.reg$one.step.prediction.errors[-(1:burn),])

library(ggpubr)
ggqqplot(bsts_res)
# 
# 
shapiro.test(bsts_res)


acf(bsts_res)
Box.test(bsts_res, type = c("Ljung-Box"))
bds.test(bsts_res) 
#fNonlinear::bdsTest(bsts_res) 


### Predict
p <- predict.bsts(bsts.reg.priors , horizon = num_prediction,newdata=as.ts(data_test[,-1]), burn = burn, quantiles = c(.025, .975))

#forcasted_date<-seq(time(data_train)[nrow(data_train)]+1, by = "day", length.out = num_prediction)

forcasted_date<-time(data_test)



### Actual versus predicted
d2 <- data.frame(as.numeric(-colMeans(bsts.reg.priors$one.step.prediction.errors[-(1:burn),])+as.ts(data_train$adjClose)),
                 time(data_train))
names(d2) <- c("Fitted","Date")




MAPE11_train<-MAPE(as.numeric(d2$Fitted),as.numeric(data_train$adjClose))*100
forcasted_Linier<-as.numeric(p$mean)
forcasted_T<-forcasted_Linier




d2_twitter_google <- data.frame(
  # fitted values and predictions
  # actual data and dates 
  as.numeric(c(coredata(data_train$adjClose),forcasted_T)),
  as.numeric(coredata(data$adjClose)),
  c(time(data_train),forcasted_date)
)


names(d2_twitter_google) <- c("forcast","test", "Date")



MAPE11<-c(MAPE11,MAPE(forcasted_T,data_test$adjClose))
}


plot(c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8),MAPE11[1:8],type="l",lwd=1.8, col = "dark red",xlab="pi",ylab="MAPE")



######################for only google data#######################
MAPE111<-c()
for(i in c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)){
prior <-SpikeSlabPrior(x=model.matrix(adjClose~., data=as.ts(data_train[,-c(2:8)]) ), 
                       y=as.ts(data_train$adjClose),
                       expected.r2 = 0.6,
                       expected.model.size = 10,  # expect 3 nonzero predictors
                       prior.inclusion.probabilities=rep(i,ncol(data_train[,-c(2:8)])),
                       #prior.df = .01,           # weaker prior than the default
                       diagonal.shrinkage = 0# use Zellner's prior
)




ss<-list()
#ss <-AddSemilocalLinearTrend(ss,as.ts(train_data))
#ss <-AddLocalLevel(ss,as.ts(data_train$adjClose))
#ss <- AddMonthlyAnnualCycle(ss,data_train)
ss <-AddLocalLinearTrend(ss,as.ts(data_train$adjClose) )
ss <- AddSeasonal(ss, as.ts(data_train$adjClose), nseasons =52,season.duration=7)

bsts.reg.priors <- bsts(adjClose~., state.specification = ss,data=as.ts(data_train[,-c(2:8)]),niter = 10000,prior=prior, seed=123)
burn <-9000



bsts_res<- as.numeric(residuals(bsts.reg.priors,burn=burn,mean.only=T))

#bsts_res<-colMeans(bsts.reg$one.step.prediction.errors[-(1:burn),])

### Predict
p <- predict.bsts(bsts.reg.priors , horizon = num_prediction,newdata=as.ts(data_test[,-c(1:8)]), burn = burn, quantiles = c(.025, .975))

#forcasted_date<-seq(time(data_train)[nrow(data_train)]+1, by = "day", length.out = num_prediction)

forcasted_date<-time(data_test)



### Actual versus predicted
d2 <- data.frame(as.numeric(-colMeans(bsts.reg.priors$one.step.prediction.errors[-(1:burn),])+as.ts(data_train$adjClose)),
                 time(data_train))
names(d2) <- c("Fitted","Date")




MAPE11_train<-MAPE(as.numeric(d2$Fitted),as.numeric(data_train$adjClose))*100



forcasted_Linier<-as.numeric(p$mean)
forcasted_T<-forcasted_Linier




d2 <- data.frame(
  # fitted values and predictions
  # actual data and dates 
  as.numeric(c(coredata(data_train$adjClose),forcasted_T)),
  as.numeric(coredata(data$adjClose)),
  c(time(data_train),forcasted_date)
)


names(d2) <- c("forcast","test", "Date")


MAPE111<-c(MAPE111,MAPE(forcasted_T,data_test$adjClose))

}

c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)[which.min(MAPE111)]
MAPE111

############################twitter###############################
MAPE1111<-c()
for(i in c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)){

prior <-SpikeSlabPrior(x=model.matrix(adjClose~., data=as.ts(data_train[,1:8]) ), 
                       y=as.ts(data_train$adjClose),
                       expected.r2 = 0.6,
                       expected.model.size = 2,  # expect 3 nonzero predictors
                       #prior.df = .01,           # weaker prior than the default
                       prior.inclusion.probabilities=rep(i,ncol(data_train[,1:8])),
                       diagonal.shrinkage = 0# use Zellner's prior
)



ss<-list()
#ss <-AddSemilocalLinearTrend(ss,as.ts(train_data))
#ss <-AddLocalLevel(ss,as.ts(data_train$adjClose))
#ss <- AddMonthlyAnnualCycle(ss,data_train)
ss <-AddLocalLinearTrend(ss,as.ts(data_train$adjClose) )
ss <- AddSeasonal(ss, as.ts(data_train$adjClose),nseasons =52,season.duration=7)

bsts.reg.priors <- bsts(adjClose~., state.specification = ss,data=as.ts(data_train[,1:8]),prior=prior,niter = 10000, seed=123)
burn <-9000


### Predict
p <- predict.bsts(bsts.reg.priors , horizon = num_prediction,newdata=as.ts(data_test[,c(2:8)]), burn = burn, quantiles = c(.025, .975))

#forcasted_date<-seq(time(data_train)[nrow(data_train)]+1, by = "day", length.out = num_prediction)

forcasted_date<-time(data_test)



### Actual versus predicted
d2 <- data.frame(as.numeric(-colMeans(bsts.reg.priors$one.step.prediction.errors[-(1:burn),])+as.ts(data_train$adjClose)),
                 time(data_train))
names(d2) <- c("Fitted","Date")



forcasted_Linier<-as.numeric(p$mean)
forcasted_T<-forcasted_Linier




d2_twitter <- data.frame(
  # fitted values and predictions
  # actual data and dates 
  as.numeric(c(coredata(data_train$adjClose),forcasted_T)),
  as.numeric(coredata(data$adjClose)),
  c(time(data_train),forcasted_date)
)


names(d2_twitter) <- c("forcast","test", "Date")


MAPE1111<-c(MAPE1111,MAPE(forcasted_T,data_test$adjClose))

}

c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)[which.min(MAPE1111)]
MAPE1111

