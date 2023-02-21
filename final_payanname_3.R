

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

prior <-SpikeSlabPrior(x=model.matrix(adjClose~., data=as.ts(data_train) ),
                       y=as.ts(data_train$adjClose),
                       expected.r2 = 0.6,
                       expected.model.size =10,  # expect 3 nonzero predictors
                       #prior.df = .01,           # weaker prior than the default
                       prior.inclusion.probabilities=rep(0.3,ncol(data_train)),
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

summary(bsts.reg.priors,burn=burn)

view(summary(bsts.reg.priors,burn=burn)$coefficients)

a <- bsts.reg.priors$one.step.prediction.error
plot(rowMeans(a ^ 2), type = "l")


plot(bsts.reg.priors$sigma.trend.level, type = "l")


plot(bsts.reg.priors$log.likelihood, type = "l")



library(loo)

waic_BST_LSTM_by_2regrssor<-waic(as.matrix(bsts.reg.priors$log.likelihood))




### Helper function to get the positive mean of a vector
PositiveMean <- function(b) {
  b <- b[abs(b) > 0]
  if (length(b) > 0) 
    return(mean(b))
  return(0)
}





### Get the average coefficients when variables were selected (non-zero slopes)
coeff <- data.frame(reshape2::melt(apply(bsts.reg.priors$coefficients[-(1:burn),], 2, PositiveMean)))
coeff <-filter(coeff,(coeff)!= 0)
coeff$Variable <- as.character(row.names(coeff))

dev.new(width = 1000, height = 1000, unit = "px")

ggplot(data=coeff, aes(x=Variable, y=value)) + 
  geom_bar(stat="identity", position="identity") + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) +
  xlab("") + ylab("") + ggtitle("Average coefficients")






dev.new(width = 1000, height = 1000, unit = "px")

inclusionprobs <- reshape2::melt(colMeans(bsts.reg.priors$coefficients[-(1:burn),])[colMeans(bsts.reg.priors$coefficients[-(1:burn),])!= 0])
inclusionprobs$Variable <- as.character(row.names(inclusionprobs))
ggplot(data=inclusionprobs, aes(x=Variable, y=value)) + 
  geom_bar(stat="identity", position="identity") + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) + 
  xlab("") + ylab("") + ggtitle("Inclusion probabilities")




PlotBstsCoefficients(bsts.reg.priors, burn = burn, number.of.variables =30)



###################################################################


dev.new(width = 1000, height = 1000, unit = "px")

inclusionprobs <- reshape2::melt(summary(bsts.reg.priors,burn=burn)$coefficients[,"inc.prob"][summary(bsts.reg.priors,burn=burn)$coefficients[,"inc.prob"]>0.1])
inclusionprobs$Variable <- as.character(row.names(inclusionprobs))
ggplot(data=inclusionprobs, aes(x=Variable, y=value)) + 
  geom_bar(stat="identity", position="identity") + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) + 
  xlab("") + ylab("") + ggtitle("Inclusion probabilities")


####################################################################

bsts.reg.priors$state.contributions[,3,]

state.contributions<-data.frame(day=1:311,Trend=colMeans(bsts.reg.priors$state.contributions[,1,][-(1:burn),]),Seasonal=colMeans(bsts.reg.priors$state.contributions[,2,][-(1:burn),]),Regression=colMeans(bsts.reg.priors$state.contributions[,3,][-(1:burn),]))

library(patchwork)
ggplot1 <- ggplot(state.contributions) + 
  geom_line(aes(x = day, y = Trend),size = 0.7)
ggplot2<- ggplot(state.contributions) + 
  geom_line(aes(x = day, y = Seasonal),size = 0.7)

ggplot3<- ggplot(state.contributions) + 
  geom_line(aes(x = day, y = Regression),size =0.7)

ggplot1+ggplot2+ ggplot3+plot_layout(nrow = 3)



plot(1:311,colMeans(bsts.reg.priors$state.contributions[,1,][-(1:burn),]),type="l")
plot(1:311,colMeans(bsts.reg.priors$state.contributions[,2,][-(1:burn),]),type="l")
plot(1:311,colMeans(bsts.reg.priors$state.contributions[,3,][-(1:burn),]),type="l")




plot(bsts.reg.priors, y = c("size"),burn=burn)


############################################################

dfforggplot<-data.frame(x=1:311,y1=normalize(data_train$adjClose),y2=as.numeric(data_train[,87]),y3=as.numeric(data_train[,25]),y4=as.numeric(data_train$sentiment_BourseNews))
colnames(dfforggplot)<-c("x","y1","y2","y3","y4")


pp1<-ggplot(data = dfforggplot, aes(x = x)) +
  geom_line(aes(y = y1, colour = "شاخص کل"),size=0.7) +
  geom_line(aes(y = y2, colour = "جست و جوی کلیدواژه آمریکا"),size=0.7) +
  scale_colour_manual("", 
                      breaks = c("جست و جوی کلیدواژه آمریکا","شاخص کل"),
                      values = c("#0072B2","black")) + xlab(" ")+scale_y_continuous("scaled value")



pp2<-ggplot(data = dfforggplot, aes(x = x)) +
  geom_line(aes(y = y1, colour = "شاخص کل"),size=0.7) +
  geom_line(aes(y = y3, colour = "جست و جوی کلیدواژه سهامیاب"),size=0.7) +
  scale_colour_manual("", 
                      breaks = c("جست و جوی کلیدواژه سهامیاب","شاخص کل"),
                      values = c("#0072B2","black")) + xlab(" ")+scale_y_continuous("scaled value")




pp3<-ggplot(data = dfforggplot, aes(x = x)) +
  geom_line(aes(y = y1, colour = "شاخص کل"),size=0.7) +
  geom_line(aes(y = y4, colour = "احساسات خبری بورس نیوز"),size=0.7) +
  scale_colour_manual("", 
                      breaks = c("احساسات خبری بورس نیوز","شاخص کل"),
                      values = c("#0072B2","black")) + xlab(" ")+scale_y_continuous("scaled value")


pp1+pp2+pp3+plot_layout(nrow = 3)

#######################################################3
plot(1:311 ,data_train$adjClose ,type="l",col="blue",xlab="pi",ylab="MAPE",lwd = 3)
par(new=TRUE)
plot(1:311,data_train[,87],type="l")
#######################################################



PlotBstsComponents(bsts.reg.priors,burn=burn)

plot(bsts.reg.priors, y = c("predictors"),burn=burn)   



plot(bsts.reg.priors, y = c("components"),burn=burn)

plot(bsts.reg.priors, y = c("residuals"))

plot(bsts.reg.priors, y = c("coefficients"))

plot(bsts.reg.priors, y = c("prediction.errors"))

plot(bsts.reg.priors, y = c("forecast.distribution"))

plot(bsts.reg.priors, y = c("predictors"),burn=burn)



plot(bsts.reg.priors, y = c("seasonal"))

plot(bsts.reg.priors, y = c("help"))




PlotBstsState(bsts.reg.priors,burn=burn)


####################################################
dev.new(width = 1000, height = 1000, unit = "px")



beta <- bsts.reg.priors$coefficients
if (burn > 0) {
  beta <- beta[-(1:burn), , drop = FALSE]
}
inclusion.probabilities <- colMeans(beta != 0)

inclusion.threshold=0.1

keep <- inclusion.probabilities > inclusion.threshold



inclusionprobs <- reshape2::melt(inclusion.probabilities)
inclusionprobs$Variable <- as.character(row.names(inclusionprobs))
ggplot(data=inclusionprobs, aes(x=Variable, y=value)) + 
  geom_bar(stat="identity", position="identity") + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) + 
  xlab("") + ylab("") + ggtitle("Inclusion probabilities")




###################################################




#install.packages("fNonlinear")
library(fNonlinear)
library(tseries)




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




length(as.numeric(d2$Fitted))

length(as.numeric(data_train$adjClose))


indf_data <-bsts_res
indf_data <-na.omit(indf_data)
head(indf_data)
tail(indf_data)


# Load the necessary packages
library(keras)
library(tensorflow)
MAPE11<-c()
for(s in c(2,10,20,30,40)){

step=s

a = c(coredata(indf_data), replicate(step, tail(coredata(indf_data), 1)))


N<-length(indf_data)
x = NULL
y = NULL
for(i in 1:N)
{
  s = i-1+step
  x = rbind(x,a[i:s])
  y = rbind(y,a[s+1])
}

cbind(head(x), head(y))
X = array(x, dim=c(N, step,1))

set.seed(123)
set_random_seed(123, disable_gpu = TRUE)

model = keras_model_sequential() %>%   
  layer_lstm(units=128, input_shape=c(step, 1),activation="relu") %>%
  layer_dense(units=64, activation = "relu") %>%  
  layer_dense(units=32) %>% 
  layer_dense(units=1, activation = "linear")

model %>% compile(loss = 'mse',
                  optimizer = 'adam',
                  metrics = list("mean_absolute_error")
)

model %>% summary()




model %>% fit(X,y, epochs=50, batch_size=32, shuffle = FALSE)
y_pred = model %>% predict(X)

scores = model %>% evaluate(X, y, verbose = 0)
print(scores)



prediction_list<-coredata(indf_data)[(length(indf_data)-step+1):length(indf_data)]



for(i in 1:num_prediction){
  x=prediction_list[(length(prediction_list)-step+1):length(prediction_list)]
  dim(x) = c(1,step,1)
  out = model %>% predict(x)
  prediction_list<-c(prediction_list,out)
  
}
prediction_list<-prediction_list[(step+1):length(prediction_list)]


forcasted<-prediction_list




forcasted_Linier<-as.numeric(p$mean)
forcasted_T<-forcasted_Linier+forcasted




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

step=c(2,10,20,30,40)[which.min(MAPE11)]

a = c(coredata(indf_data), replicate(step, tail(coredata(indf_data), 1)))


N<-length(indf_data)
x = NULL
y = NULL
for(i in 1:N)
{
  s = i-1+step
  x = rbind(x,a[i:s])
  y = rbind(y,a[s+1])
}

cbind(head(x), head(y))
X = array(x, dim=c(N, step,1))

set.seed(123)
set_random_seed(123, disable_gpu = TRUE)

model = keras_model_sequential() %>%   
  layer_lstm(units=128, input_shape=c(step, 1),activation="relu") %>%
  layer_dense(units=64, activation = "relu") %>%  
  layer_dense(units=32) %>% 
  layer_dense(units=1, activation = "linear")

model %>% compile(loss = 'mse',
                  optimizer = 'adam',
                  metrics = list("mean_absolute_error")
)

model %>% summary()




model %>% fit(X,y, epochs=50, batch_size=32, shuffle = FALSE)
y_pred = model %>% predict(X)

scores = model %>% evaluate(X, y, verbose = 0)
print(scores)



prediction_list<-coredata(indf_data)[(length(indf_data)-step+1):length(indf_data)]



for(i in 1:num_prediction){
  x=prediction_list[(length(prediction_list)-step+1):length(prediction_list)]
  dim(x) = c(1,step,1)
  out = model %>% predict(x)
  prediction_list<-c(prediction_list,out)
  
}
prediction_list<-prediction_list[(step+1):length(prediction_list)]


forcasted<-prediction_list




forcasted_Linier<-as.numeric(p$mean)
forcasted_T<-forcasted_Linier+forcasted




d2_twitter_google <- data.frame(
  # fitted values and predictions
  # actual data and dates 
  as.numeric(c(coredata(data_train$adjClose),forcasted_T)),
  as.numeric(coredata(data$adjClose)),
  c(time(data_train),forcasted_date)
)


names(d2_twitter_google) <- c("forcast","test", "Date")


MAPE11<-MAPE(forcasted_T,data_test$adjClose)
RMSE11<-RMSE(forcasted_T,data_test$adjClose)
MAE11<-MAE(forcasted_T,data_test$adjClose)
####################################################################
library(plotly)

fig <- plot_ly(d2_twitter_google, type = 'scatter', mode = 'lines', width = 900)%>%
  add_trace(x = ~Date[1:(length(d2_twitter_google$Date)-num_prediction)], y = ~forcast[1:(length(d2_twitter_google$forcast)-num_prediction)],name ='train')%>%
  add_trace(x = ~Date[(length(d2_twitter_google$Date)-num_prediction):length(d2_twitter_google$Date)], y = ~forcast[(length(d2_twitter_google$forcast)-num_prediction):length(d2_twitter_google$forcast)],name = 'forcasted')%>%
  add_trace(x = ~Date[(length(d2_twitter_google$Date)-num_prediction):length(d2_twitter_google$Date)], y = ~test[(length(d2_twitter_google$test)-num_prediction):length(d2_twitter_google$test)],name = 'test')%>%
  layout(showlegend = T)
fig <- fig %>%
  layout( title =paste("MAPE=%",round(MAPE11*100,5),"      " ,"BST_LSTM model by google and twitter data"),
          xaxis = list(zerolinecolor = '#ffff',
                       title = 'Date',
                       zerolinewidth = 2,
                       gridcolor = 'ffff'),
          yaxis = list(zerolinecolor = '#ffff',
                       title = 'Value',
                       zerolinewidth = 2,
                       gridcolor = 'ffff'),
          plot_bgcolor='#e5ecf6')

fig


#######################LSTM_BSTS without regrssion########################


ss<-list()
ss <-AddLocalLinearTrend(ss,as.ts(data_train$adjClose) )
#ss <-AddSemilocalLinearTrend(ss,as.ts(data_train$adjClose))
#ss <-AddLocalLevel(ss,as.ts(data_train$adjClose) )
#ss <- AddMonthlyAnnualCycle(ss,as.ts(data_train$adjClose))
ss <- AddSeasonal(ss, as.ts(data_train$adjClose), nseasons = 52,season.duration=7)

bsts.reg <- bsts(as.ts(data_train$adjClose), state.specification = ss ,niter =10000, seed=123)
burn <-9000

### Get a suggested number of burn-ins
#burn <- SuggestBurn(0.8,bsts.reg)

waic_LSTM_BSTS_without_regrssion<-waic(as.matrix(bsts.reg$log.likelihood))






#install.packages("fNonlinear")
library(fNonlinear)
library(tseries)




bsts_res<- as.numeric(residuals(bsts.reg,burn=burn,mean.only=T))

#bsts_res<-colMeans(bsts.reg$one.step.prediction.errors[-(1:burn),])



acf(bsts_res)


library(ggpubr)
ggqqplot(bsts_res)


shapiro.test(bsts_res)
Box.test(bsts_res, type = c("Ljung-Box"))
bds.test(bsts_res) 
fNonlinear::bdsTest(bsts_res) 










### Predict
p <- predict.bsts(bsts.reg , horizon = num_prediction, burn = burn, quantiles = c(.025, .975))

#forcasted_date<-seq(time(data_train)[nrow(data_train)]+1, by = "day", length.out = num_prediction)

forcasted_date<-time(data_test)



### Actual versus predicted
d2 <- data.frame(as.numeric(-colMeans(bsts.reg$one.step.prediction.errors[-(1:burn),])+as.ts(data_train$adjClose)),
                 time(data_train))
names(d2) <- c("Fitted","Date")




MAPE2_train<-MAPE(as.numeric(d2$Fitted),as.numeric(data_train$adjClose))*100



indf_data <-bsts_res
indf_data <-na.omit(indf_data)
head(indf_data)
tail(indf_data)


# Load the necessary packages
library(keras)
library(tensorflow)


MAPE2<-c()
for(s in c(2,10,20,30,40)){
  
  step=s
  
  a = c(coredata(indf_data), replicate(step, tail(coredata(indf_data), 1)))
  
  
  N<-length(indf_data)
  x = NULL
  y = NULL
  for(i in 1:N)
  {
    s = i-1+step
    x = rbind(x,a[i:s])
    y = rbind(y,a[s+1])
  }
  
  cbind(head(x), head(y))
  X = array(x, dim=c(N, step,1))
  
  set.seed(123)
  set_random_seed(123, disable_gpu = TRUE)
  
  model = keras_model_sequential() %>%   
    layer_lstm(units=128, input_shape=c(step, 1),activation="relu") %>%
    layer_dense(units=64, activation = "relu") %>%  
    layer_dense(units=32) %>% 
    layer_dense(units=1, activation = "linear")
  
  model %>% compile(loss = 'mse',
                    optimizer = 'adam',
                    metrics = list("mean_absolute_error")
  )
  
  model %>% summary()
  
  
  
  
  model %>% fit(X,y, epochs=50, batch_size=32, shuffle = FALSE)
  y_pred = model %>% predict(X)
  
  scores = model %>% evaluate(X, y, verbose = 0)
  print(scores)
  
  
  
  prediction_list<-coredata(indf_data)[(length(indf_data)-step+1):length(indf_data)]
  
  
  
  for(i in 1:num_prediction){
    x=prediction_list[(length(prediction_list)-step+1):length(prediction_list)]
    dim(x) = c(1,step,1)
    out = model %>% predict(x)
    prediction_list<-c(prediction_list,out)
    
  }
  prediction_list<-prediction_list[(step+1):length(prediction_list)]
  
  
  forcasted<-prediction_list
  
  
  
  
  forcasted_Linier<-as.numeric(p$mean)
  forcasted_T<-forcasted_Linier+forcasted
  
  
  
  
  d2_twitter_google <- data.frame(
    # fitted values and predictions
    # actual data and dates 
    as.numeric(c(coredata(data_train$adjClose),forcasted_T)),
    as.numeric(coredata(data$adjClose)),
    c(time(data_train),forcasted_date)
  )
  
  
  names(d2_twitter_google) <- c("forcast","test", "Date")
  
  
  MAPE2<-c(MAPE2,MAPE(forcasted_T,data_test$adjClose))
}

step=c(2,10,20,30,40)[which.min(MAPE2)]


a = c(coredata(indf_data), replicate(step, tail(coredata(indf_data), 1)))


N<-length(indf_data)
x = NULL
y = NULL
for(i in 1:N)
{
  s = i-1+step
  x = rbind(x,a[i:s])
  y = rbind(y,a[s+1])
}

cbind(head(x), head(y))
X = array(x, dim=c(N, step,1))

set.seed(123)
set_random_seed(123, disable_gpu = TRUE)

model = keras_model_sequential() %>%   
  layer_lstm(units=128, input_shape=c(step, 1),activation="relu") %>%
  layer_dense(units=64, activation = "relu") %>%  
  layer_dense(units=32) %>% 
  layer_dense(units=1, activation = "linear")

model %>% compile(loss = 'mse',
                  optimizer = 'adam',
                  metrics = list("mean_absolute_error")
)

model %>% summary()




model %>% fit(X,y, epochs=50, batch_size=32, shuffle = FALSE,verbose=0)
y_pred = model %>% predict(X)

scores = model %>% evaluate(X, y, verbose = 0)
print(scores)



prediction_list<-coredata(indf_data)[(length(indf_data)-step+1):length(indf_data)]



for(i in 1:num_prediction){
  x=prediction_list[(length(prediction_list)-step+1):length(prediction_list)]
  dim(x) = c(1,step,1)
  out = model %>% predict(x)
  prediction_list<-c(prediction_list,out)
  
}
prediction_list<-prediction_list[(step+1):length(prediction_list)]


forcasted<-prediction_list




forcasted_Linier<-as.numeric(p$mean)
forcasted_T<-forcasted_Linier#+forcasted




d2 <- data.frame(
  # fitted values and predictions
  # actual data and dates 
  as.numeric(c(coredata(data_train$adjClose),forcasted_T)),
  as.numeric(coredata(data$adjClose)),
  c(time(data_train),forcasted_date)
)


names(d2) <- c("forcast","test", "Date")


MAPE2<-MAPE(forcasted_T,data_test$adjClose)
RMSE2<-RMSE(forcasted_T,data_test$adjClose)
MAE2<-MAE(forcasted_T,data_test$adjClose)
####################################################################
library(plotly)

fig <- plot_ly(d2, type = 'scatter', mode = 'lines', width = 900)%>%
  add_trace(x = ~Date[1:(length(d2$Date)-num_prediction)], y = ~forcast[1:(length(d2$forcast)-num_prediction)],name ='train')%>%
  add_trace(x = ~Date[(length(d2$Date)-num_prediction):length(d2$Date)], y = ~forcast[(length(d2$forcast)-num_prediction):length(d2$forcast)],name = 'forcasted')%>%
  add_trace(x = ~Date[(length(d2$Date)-num_prediction):length(d2$Date)], y = ~test[(length(d2$test)-num_prediction):length(d2$test)],name = 'test')%>%
  layout(showlegend = T)
fig <- fig %>%
  layout( title =paste("MAPE=%",round(MAPE2*100,5),"      " ,"BSTS_LSTM model without regrssion"),
          xaxis = list(zerolinecolor = '#ffff',
                       title = 'Date',
                       zerolinewidth = 2,
                       gridcolor = 'ffff'),
          yaxis = list(zerolinecolor = '#ffff',
                       title = 'Value',
                       zerolinewidth = 2,
                       gridcolor = 'ffff'),
          plot_bgcolor='#e5ecf6')

fig




############################twitter###############################

prior <-SpikeSlabPrior(x=model.matrix(adjClose~., data=as.ts(data_train[,1:8]) ), 
                       y=as.ts(data_train$adjClose),
                       expected.r2 = 0.6,
                       expected.model.size = 2,  # expect 3 nonzero predictors
                       #prior.df = .01,           # weaker prior than the default
                       prior.inclusion.probabilities=rep(0.5,ncol(data_train[,1:8])),
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

summary(bsts.reg.priors)
view(summary(bsts.reg.priors,burn=burn)$coefficients)

a <- bsts.reg.priors$one.step.prediction.error
plot(rowMeans(a ^ 2), type = "l")


plot(bsts.reg.priors$sigma.trend.level, type = "l")


plot(bsts.reg.priors$log.likelihood, type = "l")



library(loo)



waic_BST_LSTM_twitter<-waic(as.matrix(bsts.reg.priors$log.likelihood))




### Helper function to get the positive mean of a vector
PositiveMean <- function(b) {
  b <- b[abs(b) > 0]
  if (length(b) > 0) 
    return(mean(b))
  return(0)
}





### Get the average coefficients when variables were selected (non-zero slopes)
coeff <- data.frame(reshape2::melt(apply(bsts.reg.priors$coefficients[-(1:burn),], 2, PositiveMean)))
coeff <-filter(coeff,(coeff)!= 0)
coeff$Variable <- as.character(row.names(coeff))

dev.new(width = 1000, height = 1000, unit = "px")

ggplot(data=coeff, aes(x=Variable, y=value)) + 
  geom_bar(stat="identity", position="identity") + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) +
  xlab("") + ylab("") + ggtitle("Average coefficients")






dev.new(width = 1000, height = 1000, unit = "px")

inclusionprobs <- reshape2::melt(colMeans(bsts.reg.priors$coefficients[-(1:burn),])[colMeans(bsts.reg.priors$coefficients[-(1:burn),])!= 0])
inclusionprobs$Variable <- as.character(row.names(inclusionprobs))
ggplot(data=inclusionprobs, aes(x=Variable, y=value)) + 
  geom_bar(stat="identity", position="identity") + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) + 
  xlab("") + ylab("") + ggtitle("Inclusion probabilities")







PlotBstsCoefficients(bsts.reg.priors, burn = burn, number.of.variables =30)






####################################################
dev.new(width = 1000, height = 1000, unit = "px")



beta <- bsts.reg.priors$coefficients
if (burn > 0) {
  beta <- beta[-(1:burn), , drop = FALSE]
}
inclusion.probabilities <- colMeans(beta != 0)

inclusion.threshold=0.1

keep <- inclusion.probabilities > inclusion.threshold



inclusionprobs <- reshape2::melt(inclusion.probabilities)
inclusionprobs$Variable <- as.character(row.names(inclusionprobs))
ggplot(data=inclusionprobs, aes(x=Variable, y=value)) + 
  geom_bar(stat="identity", position="identity") + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) + 
  xlab("") + ylab("") + ggtitle("Inclusion probabilities")







#install.packages("fNonlinear")
library(fNonlinear)
library(tseries)




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
p <- predict.bsts(bsts.reg.priors , horizon = num_prediction,newdata=as.ts(data_test[,c(2:8)]), burn = burn, quantiles = c(.025, .975))

#forcasted_date<-seq(time(data_train)[nrow(data_train)]+1, by = "day", length.out = num_prediction)

forcasted_date<-time(data_test)



### Actual versus predicted
d2 <- data.frame(as.numeric(-colMeans(bsts.reg.priors$one.step.prediction.errors[-(1:burn),])+as.ts(data_train$adjClose)),
                 time(data_train))
names(d2) <- c("Fitted","Date")




MAPE11_train<-MAPE(as.numeric(d2$Fitted),as.numeric(data_train$adjClose))*100




length(as.numeric(d2$Fitted))

length(as.numeric(data_train$adjClose))


indf_data <-bsts_res
indf_data <-na.omit(indf_data)
head(indf_data)
tail(indf_data)


# Load the necessary packages
library(keras)
library(tensorflow)


step=40

a = c(coredata(indf_data), replicate(step, tail(coredata(indf_data), 1)))


N<-length(indf_data)
x = NULL
y = NULL
for(i in 1:N)
{
  s = i-1+step
  x = rbind(x,a[i:s])
  y = rbind(y,a[s+1])
}

cbind(head(x), head(y))
X = array(x, dim=c(N, step,1))

set.seed(123)
set_random_seed(123, disable_gpu = TRUE)

model = keras_model_sequential() %>%   
  layer_lstm(units=128, input_shape=c(step, 1),activation="relu") %>%
  layer_dense(units=64, activation = "relu") %>%  
  layer_dense(units=32) %>% 
  layer_dense(units=1, activation = "linear")

model %>% compile(loss = 'mse',
                  optimizer = 'adam',
                  metrics = list("mean_absolute_error")
)

model %>% summary()




model %>% fit(X,y, epochs=50, batch_size=16, shuffle = FALSE,verbose=0)
y_pred = model %>% predict(X)

scores = model %>% evaluate(X, y, verbose = 0)
print(scores)



prediction_list<-coredata(indf_data)[(length(indf_data)-step+1):length(indf_data)]



for(i in 1:num_prediction){
  x=prediction_list[(length(prediction_list)-step+1):length(prediction_list)]
  dim(x) = c(1,step,1)
  out = model %>% predict(x)
  prediction_list<-c(prediction_list,out)
  
}
prediction_list<-prediction_list[(step+1):length(prediction_list)]


forcasted<-prediction_list




forcasted_Linier<-as.numeric(p$mean)
forcasted_T<-forcasted_Linier+forcasted




d2_twitter <- data.frame(
  # fitted values and predictions
  # actual data and dates 
  as.numeric(c(coredata(data_train$adjClose),forcasted_T)),
  as.numeric(coredata(data$adjClose)),
  c(time(data_train),forcasted_date)
)


names(d2_twitter) <- c("forcast","test", "Date")


MAPE11<-MAPE(forcasted_T,data_test$adjClose)
RMSE11<-RMSE(forcasted_T,data_test$adjClose)
MAE11<-MAE(forcasted_T,data_test$adjClose)
####################################################################
library(plotly)

fig <- plot_ly(d2_twitter, type = 'scatter', mode = 'lines', width = 900)%>%
  add_trace(x = ~Date[1:(length(d2_twitter$Date)-num_prediction)], y = ~forcast[1:(length(d2_twitter$forcast)-num_prediction)],name ='train')%>%
  add_trace(x = ~Date[(length(d2_twitter$Date)-num_prediction):length(d2_twitter$Date)], y = ~forcast[(length(d2_twitter$forcast)-num_prediction):length(d2_twitter$forcast)],name = 'forcasted')%>%
  add_trace(x = ~Date[(length(d2_twitter$Date)-num_prediction):length(d2_twitter$Date)], y = ~test[(length(d2_twitter$test)-num_prediction):length(d2_twitter$test)],name = 'test')%>%
  layout(showlegend = T)
fig <- fig %>%
  layout( title =paste("MAPE=%",round(MAPE11*100,5),"      " ,"BSTS_LSTM model by twitter data"),
          xaxis = list(zerolinecolor = '#ffff',
                       title = 'Date',
                       zerolinewidth = 2,
                       gridcolor = 'ffff'),
          yaxis = list(zerolinecolor = '#ffff',
                       title = 'Value',
                       zerolinewidth = 2,
                       gridcolor = 'ffff'),
          plot_bgcolor='#e5ecf6')

fig



################################google###############################
prior <-SpikeSlabPrior(x=model.matrix(adjClose~., data=as.ts(data_train[,-c(2:8)]) ), 
                       y=as.ts(data_train$adjClose),
                       expected.r2 = 0.6,
                       expected.model.size = 10,  # expect 3 nonzero predictors
                       prior.inclusion.probabilities=rep(0.6,ncol(data_train[,-c(2:8)])),
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

summary(bsts.reg.priors)


a <- bsts.reg.priors$one.step.prediction.error
plot(rowMeans(a ^ 2), type = "l")


plot(bsts.reg.priors$sigma.trend.level, type = "l")


plot(bsts.reg.priors$log.likelihood, type = "l")



library(loo)



waic_BST_LSTM_google<-waic(as.matrix(bsts.reg.priors$log.likelihood))




### Helper function to get the positive mean of a vector
PositiveMean <- function(b) {
  b <- b[abs(b) > 0]
  if (length(b) > 0) 
    return(mean(b))
  return(0)
}





### Get the average coefficients when variables were selected (non-zero slopes)
coeff <- data.frame(reshape2::melt(apply(bsts.reg.priors$coefficients[-(1:burn),], 2, PositiveMean)))
coeff <-filter(coeff,(coeff)!= 0)
coeff$Variable <- as.character(row.names(coeff))

dev.new(width = 1000, height = 1000, unit = "px")

ggplot(data=coeff, aes(x=Variable, y=value)) + 
  geom_bar(stat="identity", position="identity") + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) +
  xlab("") + ylab("") + ggtitle("Average coefficients")






dev.new(width = 1000, height = 1000, unit = "px")

inclusionprobs <- reshape2::melt(colMeans(bsts.reg.priors$coefficients[-(1:burn),])[colMeans(bsts.reg.priors$coefficients[-(1:burn),])!= 0])
inclusionprobs$Variable <- as.character(row.names(inclusionprobs))
ggplot(data=inclusionprobs, aes(x=Variable, y=value)) + 
  geom_bar(stat="identity", position="identity") + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) + 
  xlab("") + ylab("") + ggtitle("Inclusion probabilities")







PlotBstsCoefficients(bsts.reg.priors, burn = burn, number.of.variables =30)






####################################################
dev.new(width = 1000, height = 1000, unit = "px")



beta <- bsts.reg.priors$coefficients
if (burn > 0) {
  beta <- beta[-(1:burn), , drop = FALSE]
}
inclusion.probabilities <- colMeans(beta != 0)

inclusion.threshold=0.1

keep <- inclusion.probabilities > inclusion.threshold



inclusionprobs <- reshape2::melt(inclusion.probabilities)
inclusionprobs$Variable <- as.character(row.names(inclusionprobs))
ggplot(data=inclusionprobs, aes(x=Variable, y=value)) + 
  geom_bar(stat="identity", position="identity") + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) + 
  xlab("") + ylab("") + ggtitle("Inclusion probabilities")







#install.packages("fNonlinear")
library(fNonlinear)
library(tseries)




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
p <- predict.bsts(bsts.reg.priors , horizon = num_prediction,newdata=as.ts(data_test[,-c(1:8)]), burn = burn, quantiles = c(.025, .975))

#forcasted_date<-seq(time(data_train)[nrow(data_train)]+1, by = "day", length.out = num_prediction)

forcasted_date<-time(data_test)



### Actual versus predicted
d2 <- data.frame(as.numeric(-colMeans(bsts.reg.priors$one.step.prediction.errors[-(1:burn),])+as.ts(data_train$adjClose)),
                 time(data_train))
names(d2) <- c("Fitted","Date")




MAPE11_train<-MAPE(as.numeric(d2$Fitted),as.numeric(data_train$adjClose))*100




length(as.numeric(d2$Fitted))

length(as.numeric(data_train$adjClose))


indf_data <-bsts_res
indf_data <-na.omit(indf_data)
head(indf_data)
tail(indf_data)


# Load the necessary packages
library(keras)
library(tensorflow)

MAPE111<-c()

for(s in c(2,10,20,30,40)){
  
  step=s
  
  a = c(coredata(indf_data), replicate(step, tail(coredata(indf_data), 1)))
  
  
  N<-length(indf_data)
  x = NULL
  y = NULL
  for(i in 1:N)
  {
    s = i-1+step
    x = rbind(x,a[i:s])
    y = rbind(y,a[s+1])
  }
  
  cbind(head(x), head(y))
  X = array(x, dim=c(N, step,1))
  
  set.seed(123)
  set_random_seed(123, disable_gpu = TRUE)
  
  model = keras_model_sequential() %>%   
    layer_lstm(units=128, input_shape=c(step, 1),activation="relu") %>%
    layer_dense(units=64, activation = "relu") %>%  
    layer_dense(units=32) %>% 
    layer_dense(units=1, activation = "linear")
  
  model %>% compile(loss = 'mse',
                    optimizer = 'adam',
                    metrics = list("mean_absolute_error")
  )
  
  model %>% summary()
  
  
  
  
  model %>% fit(X,y, epochs=50, batch_size=32, shuffle = FALSE)
  y_pred = model %>% predict(X)
  
  scores = model %>% evaluate(X, y, verbose = 0)
  print(scores)
  
  
  
  prediction_list<-coredata(indf_data)[(length(indf_data)-step+1):length(indf_data)]
  
  
  
  for(i in 1:num_prediction){
    x=prediction_list[(length(prediction_list)-step+1):length(prediction_list)]
    dim(x) = c(1,step,1)
    out = model %>% predict(x)
    prediction_list<-c(prediction_list,out)
    
  }
  prediction_list<-prediction_list[(step+1):length(prediction_list)]
  
  
  forcasted<-prediction_list
  
  
  
  
  forcasted_Linier<-as.numeric(p$mean)
  forcasted_T<-forcasted_Linier+forcasted
  
  
  
  
  d2_twitter_google <- data.frame(
    # fitted values and predictions
    # actual data and dates 
    as.numeric(c(coredata(data_train$adjClose),forcasted_T)),
    as.numeric(coredata(data$adjClose)),
    c(time(data_train),forcasted_date)
  )
  
  
  names(d2_twitter_google) <- c("forcast","test", "Date")
  
  
  MAPE111<-c(MAPE111,MAPE(forcasted_T,data_test$adjClose))
}

step=c(2,10,20,30,40)[which.min(MAPE111)]



#step=40

a = c(coredata(indf_data), replicate(step, tail(coredata(indf_data), 1)))


N<-length(indf_data)
x = NULL
y = NULL
for(i in 1:N)
{
  s = i-1+step
  x = rbind(x,a[i:s])
  y = rbind(y,a[s+1])
}

cbind(head(x), head(y))
X = array(x, dim=c(N, step,1))

set.seed(123)
set_random_seed(123, disable_gpu = TRUE)

model = keras_model_sequential() %>%   
  layer_lstm(units=128, input_shape=c(step, 1),activation="relu") %>%
  layer_dense(units=64, activation = "relu") %>%  
  layer_dense(units=32) %>% 
  layer_dense(units=1, activation = "linear")

model %>% compile(loss = 'mse',
                  optimizer = 'adam',
                  metrics = list("mean_absolute_error")
)

model %>% summary()




model %>% fit(X,y, epochs=50, batch_size=32, shuffle = FALSE,verbose=0)
y_pred = model %>% predict(X)

scores = model %>% evaluate(X, y, verbose = 0)
print(scores)



prediction_list<-coredata(indf_data)[(length(indf_data)-step+1):length(indf_data)]



for(i in 1:num_prediction){
  x=prediction_list[(length(prediction_list)-step+1):length(prediction_list)]
  dim(x) = c(1,step,1)
  out = model %>% predict(x)
  prediction_list<-c(prediction_list,out)
  
}
prediction_list<-prediction_list[(step+1):length(prediction_list)]


forcasted<-prediction_list




forcasted_Linier<-as.numeric(p$mean)
forcasted_T<-forcasted_Linier+forcasted




d2 <- data.frame(
  # fitted values and predictions
  # actual data and dates 
  as.numeric(c(coredata(data_train$adjClose),forcasted_T)),
  as.numeric(coredata(data$adjClose)),
  c(time(data_train),forcasted_date)
)


names(d2) <- c("forcast","test", "Date")


MAPE111<-MAPE(forcasted_T,data_test$adjClose)
MSE111<-RMSE(forcasted_T,data_test$adjClose)
MAE111<-MAE(forcasted_T,data_test$adjClose)
####################################################################
library(plotly)

fig <- plot_ly(d2, type = 'scatter', mode = 'lines', width = 900)%>%
  add_trace(x = ~Date[1:(length(d2$Date)-num_prediction)], y = ~forcast[1:(length(d2$forcast)-num_prediction)],name ='train')%>%
  add_trace(x = ~Date[(length(d2$Date)-num_prediction):length(d2$Date)], y = ~forcast[(length(d2$forcast)-num_prediction):length(d2$forcast)],name = 'forcasted')%>%
  add_trace(x = ~Date[(length(d2$Date)-num_prediction):length(d2$Date)], y = ~test[(length(d2$test)-num_prediction):length(d2$test)],name = 'test')%>%
  layout(showlegend = T)
fig <- fig %>%
  layout( title =paste("MAPE=%",round(MAPE111*100,5),"      " ,"BSTS_LSTM model by google data"),
          xaxis = list(zerolinecolor = '#ffff',
                       title = 'Date',
                       zerolinewidth = 2,
                       gridcolor = 'ffff'),
          yaxis = list(zerolinecolor = '#ffff',
                       title = 'Value',
                       zerolinewidth = 2,
                       gridcolor = 'ffff'),
          plot_bgcolor='#e5ecf6')

fig



#######################################################################
CompareBstsModels(list("BSTS_LSTM model by regrssion" =bsts.reg.priors ,
                       "BSTS_LSTM model without regrssion" = bsts.reg),
                  colors = c("black", "red"))

##############################################################################
#########################ARIMA_LSTM
##############################################################################

arima_tr<- auto.arima(data_train$adjClose, ic="aic")
summary(arima_tr)
lmtest::coeftest(arima_tr)
forcasted_Linier<-forecast(arima_tr,h=num_prediction)$mean



indf_data <- as.numeric(arima_tr$residuals)



indf_data <-na.omit(indf_data)
head(indf_data)
tail(indf_data)


# Load the necessary packages
library(keras)
library(tensorflow)



MAPE3<-c()

for(s in c(2,10,20,30,40)){
  
  step=s
  
  a = c(coredata(indf_data), replicate(step, tail(coredata(indf_data), 1)))
  
  
  N<-length(indf_data)
  x = NULL
  y = NULL
  for(i in 1:N)
  {
    s = i-1+step
    x = rbind(x,a[i:s])
    y = rbind(y,a[s+1])
  }
  
  cbind(head(x), head(y))
  X = array(x, dim=c(N, step,1))
  
  set.seed(123)
  set_random_seed(123, disable_gpu = TRUE)
  
  model = keras_model_sequential() %>%   
    layer_lstm(units=128, input_shape=c(step, 1),activation="relu") %>%
    layer_dense(units=64, activation = "relu") %>%  
    layer_dense(units=32) %>% 
    layer_dense(units=1, activation = "linear")
  
  model %>% compile(loss = 'mse',
                    optimizer = 'adam',
                    metrics = list("mean_absolute_error")
  )
  
  model %>% summary()
  
  
  
  
  model %>% fit(X,y, epochs=50, batch_size=32, shuffle = FALSE)
  y_pred = model %>% predict(X)
  
  scores = model %>% evaluate(X, y, verbose = 0)
  print(scores)
  
  
  
  prediction_list<-coredata(indf_data)[(length(indf_data)-step+1):length(indf_data)]
  
  
  
  for(i in 1:num_prediction){
    x=prediction_list[(length(prediction_list)-step+1):length(prediction_list)]
    dim(x) = c(1,step,1)
    out = model %>% predict(x)
    prediction_list<-c(prediction_list,out)
    
  }
  prediction_list<-prediction_list[(step+1):length(prediction_list)]
  
  
  forcasted<-prediction_list
  
  
  forcasted_T<-forcasted_Linier+forcasted
  
  
  forcasted_date<-time(data_test)
  
  d3 <- data.frame(
    # fitted values and predictions
    # actual data and dates 
    as.numeric(c(coredata(data_train$adjClose),forcasted_T)),
    as.numeric(coredata(data$adjClose)),
    c(time(data_train),forcasted_date)
  )
  
  
  
  names(d3) <- c("forcast","test", "Date")

  
  MAPE3<-c(MAPE3,MAPE(as.numeric(forcasted_T),as.numeric(data_test$adjClose)))
}

step=c(2,10,20,30,40)[which.min(MAPE3)]


#step=30

a = c(coredata(indf_data), replicate(step, tail(coredata(indf_data), 1)))


N<-length(indf_data)
x = NULL
y = NULL
for(i in 1:N)
{
  s = i-1+step
  x = rbind(x,a[i:s])
  y = rbind(y,a[s+1])
}

cbind(head(x), head(y))
X = array(x, dim=c(N, step,1))

set.seed(123)
set_random_seed(123, disable_gpu = TRUE)

model = keras_model_sequential() %>%   
  layer_lstm(units=128, input_shape=c(step, 1),activation="relu") %>%
  layer_dense(units=64, activation = "relu") %>%  
  layer_dense(units=32) %>% 
  layer_dense(units=1, activation = "linear")

model %>% compile(loss = 'mse',
                  optimizer = 'adam',
                  metrics = list("mean_absolute_error")
)

model %>% summary()




model %>% fit(X,y, epochs=50, batch_size=32, shuffle = FALSE,verbose=0)
y_pred = model %>% predict(X)

scores = model %>% evaluate(X, y, verbose = 0)
print(scores)



prediction_list<-coredata(indf_data)[(length(indf_data)-step+1):length(indf_data)]



for(i in 1:num_prediction){
  x=prediction_list[(length(prediction_list)-step+1):length(prediction_list)]
  dim(x) = c(1,step,1)
  out = model %>% predict(x)
  prediction_list<-c(prediction_list,out)
  
}
prediction_list<-prediction_list[(step+1):length(prediction_list)]


forcasted<-prediction_list


forcasted_T<-forcasted_Linier+forcasted



forcasted_date<-time(data_test)
d3 <- data.frame(
  # fitted values and predictions
  # actual data and dates 
  as.numeric(c(coredata(data_train$adjClose),forcasted_T)),
  as.numeric(coredata(data$adjClose)),
  c(time(data_train),forcasted_date)
)



names(d3) <- c("forcast","test", "Date")

MAPE3<-MAPE(as.numeric(forcasted_T),as.numeric(data_test$adjClose))
MSE3<-RMSE(as.numeric(forcasted_T),as.numeric(data_test$adjClose))
MAE3<-MAE(as.numeric(forcasted_T),as.numeric(data_test$adjClose))

####################################################################
library(plotly)

fig <- plot_ly(d3, type = 'scatter', mode = 'lines', width = 900)%>%
  add_trace(x = ~Date[1:(length(d3$Date)-num_prediction)], y = ~forcast[1:(length(d3$forcast)-num_prediction)],name ='train')%>%
  add_trace(x = ~Date[(length(d3$Date)-num_prediction):length(d3$Date)], y = ~forcast[(length(d3$forcast)-num_prediction):length(d3$forcast)],name = 'forcasted')%>%
  add_trace(x = ~Date[(length(d3$Date)-num_prediction):length(d3$Date)], y = ~test[(length(d3$test)-num_prediction):length(d3$test)],name = 'test')%>%
  layout(showlegend = T)
fig <- fig %>%
  layout( title =paste("MAPE=%",round(MAPE3*100,5),"      " ,"ARIMA_LSTM"),
          xaxis = list(zerolinecolor = '#ffff',
                       title = 'Date',
                       zerolinewidth = 2,
                       gridcolor = 'ffff'),
          yaxis = list(zerolinecolor = '#ffff',
                       title = 'Value',
                       zerolinewidth = 2,
                       gridcolor = 'ffff'),
          plot_bgcolor='#e5ecf6')

fig
############################VAR_LSTM###########################

library(vars)

VARselect(cbind(diff(data_train[,1]),data_train[,2:8])[-1,], lag.max=10,type="both")[["selection"]]





var1 <- VAR(cbind(diff(data_train[,1]),data_train[,2:8])[-1,], p=1, type = "both")


#serial.test(var1, lags.pt=10, type="PT.asymptotic")
residual<-var1$varresult$adjClose$residuals

residual<-as.numeric(residual)



predictions <- predict(var1, n.ahead = num_prediction, ci = 0.95)
#plot(predictions, names = "adjClose")



da<-predictions$fcst$adjClose
da<-da[,1]


forcasted_date<-time(data_test)


residual

indf_data <-residual
indf_data <-na.omit(indf_data)


# Load the necessary packages
library(keras)
library(tensorflow)

MAPE33<-c()

for(s in c(2,10,20,30,40)){
  
  step=s
  
  a = c(coredata(indf_data), replicate(step, tail(coredata(indf_data), 1)))
  
  
  N<-length(indf_data)
  x = NULL
  y = NULL
  for(i in 1:N)
  {
    s = i-1+step
    x = rbind(x,a[i:s])
    y = rbind(y,a[s+1])
  }
  
  cbind(head(x), head(y))
  X = array(x, dim=c(N, step,1))
  
  set.seed(123)
  set_random_seed(123, disable_gpu = TRUE)
  
  model = keras_model_sequential() %>%   
    layer_lstm(units=128, input_shape=c(step, 1),activation="relu") %>%
    layer_dense(units=64, activation = "relu") %>%  
    layer_dense(units=32) %>% 
    layer_dense(units=1, activation = "linear")
  
  model %>% compile(loss = 'mse',
                    optimizer = 'adam',
                    metrics = list("mean_absolute_error")
  )
  
  model %>% summary()
  
  
  
  
  model %>% fit(X,y, epochs=50, batch_size=32, shuffle = FALSE)
  y_pred = model %>% predict(X)
  
  scores = model %>% evaluate(X, y, verbose = 0)
  print(scores)
  
  
  
  prediction_list<-coredata(indf_data)[(length(indf_data)-step+1):length(indf_data)]
  
  
  
  for(i in 1:num_prediction){
    x=prediction_list[(length(prediction_list)-step+1):length(prediction_list)]
    dim(x) = c(1,step,1)
    out = model %>% predict(x)
    prediction_list<-c(prediction_list,out)
    
  }
  prediction_list<-prediction_list[(step+1):length(prediction_list)]
  
  
  forcasted<-prediction_list
  
  
  
  
  
  forcasted_Linier<-as.numeric(da)
  forcasted_T<-forcasted_Linier+forcasted
  forcasted_T<-tail(diffinv(c(tail(coredata(data_train$adjClose),1),forcasted_T)),num_prediction)
  
  
  forcasted_date<-time(data_test)
  
  
  d4 <- data.frame(
    # fitted values and predictions
    # actual data and dates 
    as.numeric(c(coredata(data_train$adjClose),forcasted_T)),
    as.numeric(coredata(data$adjClose)),
    c(time(data_train),forcasted_date)
  )
  
  
  
  names(d4) <- c("forcast","test", "Date")
  
  MAPE33<-c(MAPE33,MAPE(as.numeric(forcasted_T),as.numeric(data_test$adjClose)))
}

step=c(2,10,20,30,40)[which.min(MAPE33)]


#step=40

a = c(coredata(indf_data), replicate(step, tail(coredata(indf_data), 1)))


N<-length(indf_data)
x = NULL
y = NULL
for(i in 1:N)
{
  s = i-1+step
  x = rbind(x,a[i:s])
  y = rbind(y,a[s+1])
}

cbind(head(x), head(y))
X = array(x, dim=c(N, step,1))

set.seed(123)
set_random_seed(123, disable_gpu = TRUE)

model = keras_model_sequential() %>%   
  layer_lstm(units=128, input_shape=c(step, 1),activation="relu") %>%
  layer_dense(units=64, activation = "relu") %>%  
  layer_dense(units=32) %>% 
  layer_dense(units=1, activation = "linear")

model %>% compile(loss = 'mse',
                  optimizer = 'adam',
                  metrics = list("mean_absolute_error")
)

model %>% summary()




model %>% fit(X,y, epochs=50, batch_size=32, shuffle = FALSE)
y_pred = model %>% predict(X)

scores = model %>% evaluate(X, y, verbose = 0)
print(scores)



prediction_list<-coredata(indf_data)[(length(indf_data)-step+1):length(indf_data)]



for(i in 1:num_prediction){
  x=prediction_list[(length(prediction_list)-step+1):length(prediction_list)]
  dim(x) = c(1,step,1)
  out = model %>% predict(x)
  prediction_list<-c(prediction_list,out)
  
}
prediction_list<-prediction_list[(step+1):length(prediction_list)]


forcasted<-prediction_list




forcasted_Linier<-as.numeric(da)
forcasted_T<-forcasted_Linier+forcasted
forcasted_T<-tail(diffinv(c(tail(coredata(data_train$adjClose),1),forcasted_T)),num_prediction)


forcasted_date<-time(data_test)


d4 <- data.frame(
  # fitted values and predictions
  # actual data and dates 
  as.numeric(c(coredata(data_train$adjClose),forcasted_T)),
  as.numeric(coredata(data$adjClose)),
  c(time(data_train),forcasted_date)
)



names(d4) <- c("forcast","test", "Date")

MAPE33<-MAPE(as.numeric(forcasted_T),as.numeric(data_test$adjClose))
RMSE33<-RMSE(as.numeric(forcasted_T),as.numeric(data_test$adjClose))
MAE33<-MAE(as.numeric(forcasted_T),as.numeric(data_test$adjClose))

####################################################################


fig <- plot_ly(d4, type = 'scatter', mode = 'lines', width = 900)%>%
  add_trace(x = ~Date[1:(length(d4$Date)-num_prediction)], y = ~forcast[1:(length(d4$forcast)-num_prediction)],name ='train')%>%
  add_trace(x = ~Date[(length(d4$Date)-num_prediction):length(d4$Date)], y = ~forcast[(length(d4$forcast)-num_prediction):length(d4$forcast)],name = 'forcasted')%>%
  add_trace(x = ~Date[(length(d4$Date)-num_prediction):length(d4$Date)], y = ~test[(length(d4$test)-num_prediction):length(d4$test)],name = 'test')%>%
  layout(showlegend = T)
fig <- fig %>%
  layout( title =paste("MAPE=%",round(MAPE33*100,5),"      " ,"VAR_LSTM"),
          xaxis = list(zerolinecolor = '#ffff',
                       title = 'Date',
                       zerolinewidth = 2,
                       gridcolor = 'ffff'),
          yaxis = list(zerolinecolor = '#ffff',
                       title = 'Value',
                       zerolinewidth = 2,
                       gridcolor = 'ffff'),
          plot_bgcolor='#e5ecf6')

fig

############################LSTM################################


library(lubridate)
library(dplyr)
library(ggplot2)
library(forecast)
library(tidyverse, quietly = TRUE)
library(DescTools)
library(quantmod)



indf_data <-coredata(data_train$adjClose)
indf_data <-na.omit(indf_data)
head(indf_data)
tail(indf_data)


# Load the necessary packages
library(keras)
library(tensorflow)


MAPE4<-c()

for(s in c(2,10,20,30,40)){
  
  step=s
  
  a = c(coredata(indf_data), replicate(step, tail(coredata(indf_data), 1)))
  
  
  N<-length(indf_data)
  x = NULL
  y = NULL
  for(i in 1:N)
  {
    s = i-1+step
    x = rbind(x,a[i:s])
    y = rbind(y,a[s+1])
  }
  
  cbind(head(x), head(y))
  X = array(x, dim=c(N, step,1))
  
  set.seed(123)
  set_random_seed(123, disable_gpu = TRUE)
  
  model = keras_model_sequential() %>%   
    layer_lstm(units=128, input_shape=c(step, 1),activation="relu") %>%
    layer_dense(units=64, activation = "relu") %>%  
    layer_dense(units=32) %>% 
    layer_dense(units=1, activation = "linear")
  
  model %>% compile(loss = 'mse',
                    optimizer = 'adam',
                    metrics = list("mean_absolute_error")
  )
  
  model %>% summary()
  
  
  
  
  model %>% fit(X,y, epochs=50, batch_size=32, shuffle = FALSE)
  y_pred = model %>% predict(X)
  
  scores = model %>% evaluate(X, y, verbose = 0)
  print(scores)
  
  
  
  prediction_list<-coredata(indf_data)[(length(indf_data)-step+1):length(indf_data)]
  
  
  
  for(i in 1:num_prediction){
    x=prediction_list[(length(prediction_list)-step+1):length(prediction_list)]
    dim(x) = c(1,step,1)
    out = model %>% predict(x)
    prediction_list<-c(prediction_list,out)
    
  }
  prediction_list<-prediction_list[(step+1):length(prediction_list)]
  
  
  forcasted<-prediction_list
  
  
  forcasted_date<-time(data_test)
  
  forcasted_T<-forcasted
  
  
  
  dlstm<- data.frame(
    # fitted values and predictions
    # actual data and dates 
    as.numeric(c(coredata(data_train$adjClose),forcasted_T)),
    as.numeric(coredata(data$adjClose)),
    c(time(data_train),forcasted_date)
  )
  
  
  names(dlstm) <- c("forcast","test", "Date")
  
  
  
  MAPE4<-c(MAPE4,MAPE(forcasted_T,coredata(data_test$adjClose)))
}

step=c(2,10,20,30,40)[which.min(MAPE4)]


#step=40

a = c(coredata(indf_data), replicate(step, tail(coredata(indf_data), 1)))


N<-length(indf_data)
x = NULL
y = NULL
for(i in 1:N)
{
  s = i-1+step
  x = rbind(x,a[i:s])
  y = rbind(y,a[s+1])
}

cbind(head(x), head(y))
X = array(x, dim=c(N, step,1))

set.seed(123)
set_random_seed(123, disable_gpu = TRUE)

model = keras_model_sequential() %>%   
  layer_lstm(units=128, input_shape=c(step, 1),activation="relu") %>%
  layer_dense(units=64, activation = "relu") %>%  
  layer_dense(units=32) %>% 
  layer_dense(units=1, activation = "linear")

model %>% compile(loss = 'mse',
                  optimizer = 'adam',
                  metrics = list("mean_absolute_error")
)

model %>% summary()




model %>% fit(X,y, epochs=50, batch_size=32, shuffle = FALSE,verbose=0)
y_pred = model %>% predict(X)

scores = model %>% evaluate(X, y, verbose = 0)
print(scores)



prediction_list<-coredata(indf_data)[(length(indf_data)-step+1):length(indf_data)]



for(i in 1:num_prediction){
  x=prediction_list[(length(prediction_list)-step+1):length(prediction_list)]
  dim(x) = c(1,step,1)
  out = model %>% predict(x)
  prediction_list<-c(prediction_list,out)
  
}
prediction_list<-prediction_list[(step+1):length(prediction_list)]


forcasted<-prediction_list



forcasted_T<-forcasted

forcasted_date<-time(data_test)


dlstm<- data.frame(
  # fitted values and predictions
  # actual data and dates 
  as.numeric(c(coredata(data_train$adjClose),forcasted_T)),
  as.numeric(coredata(data$adjClose)),
  c(time(data_train),forcasted_date)
)


names(dlstm) <- c("forcast","test", "Date")


MAPE4<-MAPE(forcasted_T,coredata(data_test$adjClose))
RMSE4<-RMSE(forcasted_T,coredata(data_test$adjClose))
MAE4<-MAE(forcasted_T,coredata(data_test$adjClose))
####################################################################
library(plotly)

fig <- plot_ly(dlstm, type = 'scatter', mode = 'lines', width = 900)%>%
  add_trace(x = ~Date[1:(length(dlstm$Date)-num_prediction)], y = ~forcast[1:(length(dlstm$forcast)-num_prediction)],name ='train')%>%
  add_trace(x = ~Date[(length(dlstm$Date)-num_prediction):length(dlstm$Date)], y = ~forcast[(length(dlstm$forcast)-num_prediction):length(dlstm$forcast)],name = 'forcasted')%>%
  add_trace(x = ~Date[(length(dlstm$Date)-num_prediction):length(dlstm$Date)], y = ~test[(length(dlstm$test)-num_prediction):length(dlstm$test)],name = 'test')%>%
  layout(showlegend = T)
fig <- fig %>%
  layout( title =paste("MAPE=%",round(MAPE4*100,5),"      " ,"LSTM model"),
          xaxis = list(zerolinecolor = '#ffff',
                       title = 'Date',
                       zerolinewidth = 2,
                       gridcolor = 'ffff'),
          yaxis = list(zerolinecolor = '#ffff',
                       title = 'Value',
                       zerolinewidth = 2,
                       gridcolor = 'ffff'),
          plot_bgcolor='#e5ecf6')

fig




############################moltyplot###########################



# dlstm<-merge(dlstm,d2,by='Date')[,1:3]
# d_T<-data.frame(Date=dlstm$Date,test=dlstm$test,forcasted_bstlstm=d2_twitter$forcast,forcasted_ARIMAlstm=d3$forcast,forcasted_VARlstm=d4$forcast)



d_T<-data.frame(Date=d2_twitter_google$Date,test=d2_twitter_google$test,forcasted_bstlstm=d2_twitter_google$forcast,forcasted_ARIMAlstm=d3$forcast,forcasted_VARlstm=d4$forcast)





library(plotly)

fig <- plot_ly(d_T, type = 'scatter', mode = 'lines', width = 1000)%>%
  add_trace(x = ~Date[1:(length(d_T$Date)-num_prediction)], y = ~forcasted_bstlstm[1:(length(d_T$forcasted_bstlstm)-num_prediction)],name ='train')%>%
  add_trace(x = ~Date[(length(d_T$Date)-num_prediction):length(d_T$Date)], y = ~test[(length(d_T$test)-num_prediction):length(d_T$test)],name = 'test')%>%
  add_trace(x = ~Date[(length(d_T$Date)-num_prediction):length(d_T$Date)], y = ~forcasted_bstlstm[(length(d_T$forcasted_bstlstm)-num_prediction):length(d_T$forcasted_bstlstm)],name = 'forcasted by BST_LSTM (proposed model)')%>%
  add_trace(x = ~Date[(length(d_T$Date)-num_prediction):length(d_T$Date)], y = ~forcasted_ARIMAlstm[(length(d_T$forcasted_ARIMAlstm)-num_prediction):length(d_T$forcasted_ARIMAlstm)],name = 'forcasted by ARIMA_LSTM')%>%
  add_trace(x = ~Date[(length(d_T$Date)-num_prediction):length(d_T$Date)], y = ~forcasted_VARlstm[(length(d_T$forcasted_VARlstm)-num_prediction):length(d_T$forcasted_VARlstm)],name = 'forcasted by VAR_LSTM')%>%
  
  layout(showlegend = T)
fig <- fig %>%
  layout( 
    xaxis = list(zerolinecolor = '#ffff',
                 title = 'Date',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    yaxis = list(zerolinecolor = '#ffff',
                 title = 'Value',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    plot_bgcolor='#e5ecf6')

fig
