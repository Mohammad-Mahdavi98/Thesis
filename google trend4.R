library(gtrendsR)
library(tidyverse)
library(trendecon)
library(tidyr)
library(dplyr)








#data2<- vector(mode = "list", length(related_queries))



saveRDS(data2,"C:\\Users\\98939\\Desktop\\data22.RData")



data2<-readRDS("C:\\Users\\98939\\Desktop\\data22.RData")





library(httr)
set_config(
  use_proxy(url="185.118.155.202", port=93443) 
)

httr::GET("www.trends.google.com")



#"http://81.12.44.197:3129/"
#"http://93.113.233.17:3129/"
proxy_url <- "http://185.118.155.202:8080/"
Sys.setenv(http_proxy = proxy_url, https_proxy = proxy_url, ftp_proxy = proxy_url)





#data=list()

for (i in 206:length(related_queries))
{
  
  data2[[i]]<-ts_gtrends_mwd( keyword = related_queries[i],
                                  geo     = "IR",
                                  from = "2020-03-10",
  )
}








trend_data<-matrix(NA,nrow=nrow(data2[[1]]),ncol=length(related_queries))

for(i in 1:length(related_queries)){
  trend_data[,i]=data2[[i]]$value[1:953]
}

trend_data<-cbind(as.data.frame(data2[[1]]$time[1:953]),trend_data)

colnames(trend_data)<-(c("Date",related_queries))


trend_data<- trend_data[!duplicated(trend_data[c('Date')]),]





saveRDS(related_queries,"C:\\Users\\98939\\Desktop\\related_queries.RData")
saveRDS(data2,"C:\\Users\\98939\\Desktop\\trend.RData")
saveRDS(trend_data,"C:\\Users\\98939\\Desktop\\trend_data2.RData")



emthan_save<-readRDS("C:\\Users\\98939\\Desktop\\trend.RData")





