# install.packages("tm")  # for text mining
# install.packages("SnowballC") # for text stemming
# install.packages("wordcloud") # word-cloud generator 
# install.packages("RColorBrewer") # color palettes
# install.packages("syuzhet") # for sentiment analysis
# install.packages("ggplot2") # for plotting graphs
# install.packages("quanteda")
# install.packages("readtext")


library("RColorBrewer")
library("NLP")
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")
library("quanteda")
library("readtext")
library("stopwords")

#rm(list=ls())
#library("readxl")
#data<-read_excel("C:\\Users\\DearUser\\Desktop\\nazrat karbaran.xlsx")

#data<-read_excel("C:\\Users\\98939\\Downloads\\20221004_Bourse24ir.csv")

data<-read.csv("C:\\Users\\98939\\Desktop\\navad_eghtesadi.csv",encoding = "UTF-8")

text<-data$tweet
#text<-iconv(text)

# Read the text file from local machine , choose file interactively
#text <- readLines(file.choose())

# Load the data as a corpus
TextDoc <- Corpus(VectorSource(text))


#############################################################

removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
TextDoc <- tm_map(TextDoc, removeURL)
inspect(TextDoc [1:5])

# removeEmoticons <- content_transformer(function(x) gsub("ًںژ¥", "", x))
# TextDoc <- tm_map(TextDoc, removeEmoticons)
# inspect(TextDoc [1:5])


removeEmoticons <- content_transformer(function (x , pattern ) gsub(pattern, "", x))
TextDoc <- tm_map(TextDoc, removeEmoticons, "ًںژ¥")
TextDoc <- tm_map(TextDoc, removeEmoticons, "ًںŒگ")
TextDoc <- tm_map(TextDoc, removeEmoticons, "ًں“ƒ")
inspect(TextDoc [1:5])



toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
inspect(TextDoc [1:5])
TextDoc <- tm_map(TextDoc, toSpace, "\n")
inspect(TextDoc [1:5])
TextDoc <- tm_map(TextDoc, removePunctuation)
inspect(TextDoc [1:5])
TextDoc <- tm_map(TextDoc, toSpace, "@")
inspect(TextDoc [1:5])
TextDoc <- tm_map(TextDoc, toSpace, "\\|")
inspect(TextDoc [1:5])
# Convert the text to lower case
TextDoc <- tm_map(TextDoc, content_transformer(tolower))
inspect(TextDoc [1:5])
# Remove numbers
TextDoc <- tm_map(TextDoc, removeNumbers)
inspect(TextDoc [1:5])
# Remove english common stopwords
#TextDoc <- tm_map(TextDoc,removeWords,stopwords::stopwords(language = "fa", source = "stopwords-iso"))
#inspect(TextDoc [1:5])

#paste0(stopwords::stopwords(language = "fa", source = "stopwords-iso"),collapse=",")
#print(stopwords::stopwords(language = "fa", source = "stopwords-iso"))


#gsub(',' , '","',paste(stopwords::stopwords(language = "fa", source = "stopwords-iso"),collapse=','))



# Remove your own stop word
# specify your custom stopwords as a character vector
# TextDoc <- tm_map(TextDoc, removeWords, c("أ›آŒأ™آ‡","أکآھأ™آˆ","أکآھأکآ§","أکآ¯أکآ±","أکآ§أکآ²","أکآ¨أکآ±"
#                                           ,"أکآ¨أکآ±أکآ§أ›آŒ"
#                                           ,"أکآ±أ™آˆ"
#                                           ,"أکآ±أکآ§"
#                                           ,"أ™آ‡أکآ±"
#                                           ,"أ™آ‡أکآ§"
#                                           ,"أڑآ†أ™آˆأ™آ†"
#                                           ,"أڑآ©أ™آ‡"
#                                           ,"أ™آ…أ™آ†"
#                                           ,"أ™آˆأ™آ„أ›آŒ"
#                                           ,"أکآ®أ›آŒأ™آ„أ›آŒ"
#                                           ,"أکآ¯أکآ§أکآ±أ™آ‡"
#                                           ,"أ™آ‡أکآ§أ›آŒ"
#                                           ,"أ›آŒأڑآ©"
#                                           ,"أڑآ†أ™آ†أکآ¯"
#                                           ,"أکآھأ™آˆأ›آŒ"
#                                           ,"أ™آ‡أکآ³أکآھ"
#                                           ,"أ™آ†أ›آŒأکآ³أکآھ"
#                                           ,"أ™آ‡أ™آ…"
#                                           ,"أکآ¨أکآ§"
#                                           ,"أکآ§أ›آŒأ™آ†"
#                                           ,"أکآ¨أ™آ‡"
#                                           ,"أکآ¨أکآ¹أکآ¯"
#                                           ,"أ™آ…أ›آŒأکآ´أ™آ‡"
#                                           ,"ط¯ط±"
#                                           ,"ط¨ظ‡"
#                                           ,"ط¨ط§"
#                                           ,"ط§ط²"
#                                           ,"ط±ط§"
#                                           ,"طھط§"
#                                           ,"ظ…غŒ")) 




TextDoc <- tm_map(TextDoc, removeWords, paste(stopwords::stopwords(language = "fa", source = "stopwords-iso"))[8:length(stopwords::stopwords(language = "fa", source = "stopwords-iso"))]) 
inspect(TextDoc [1:5])
TextDoc <- tm_map(TextDoc, removeWords, c("ط¨ط®ظˆط§ظ†غŒط¯","ط³ط§ظ„","ظ‡ظپطھظ‡","ظ…ط§ظ‡","ظ…غŒظ„غŒط§ط±ط¯","ظ…غŒظ„غŒظˆظ†","ظ‚غŒظ…طھ")) 
inspect(TextDoc [1:5])








#TextDoc <- tm_map(TextDoc, removeWords, gsub(',' , '","',paste(stopwords::stopwords(language = "fa", source = "stopwords-iso"),collapse=','))) 


















# Remove punctuations
TextDoc <- tm_map(TextDoc, removePunctuation)
inspect(TextDoc [1:5])
# Eliminate extra white spaces
TextDoc <- tm_map(TextDoc, stripWhitespace)
inspect(TextDoc [1:5])
# Text stemming - which reduces words to their root form
TextDoc <- tm_map(TextDoc, stemDocument)
inspect(TextDoc [1:5])
###################################################################
# Build a term-document matrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc)


library(quanteda)
library(readtext)

# stp_toks <- tokens(TextDoc, remove_punct = TRUE)
# stp_toks <- tokens_remove(stp_toks, stopwords::stopwords(language = "fa", source = "stopwords-iso"))
# stp_dfm <- dfm(stp_toks)


#install.packages("bigmemory")
#install.packages("biganalytics")
#install.packages("bigtabulate")
library(bigmemory)
library(biganalytics)
library(bigtabulate)
memory.limit(size = 56000)
#dtm_m<-big.matrix(TextDoc_dtm)
#install.packages("bigpca")
#library(bigpca)
#dtm_m<-import.big.data(TextDoc_dtm)
#dtm_m<-read.big.matrix(TextDoc_dtm)
dtm_m <- as.matrix(TextDoc_dtm)
# Sort by descearing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 5 most frequent words
head(dtm_d, 10)
View(head(dtm_d,10))

####################################################################

# Plot the most frequent words
dev.new(width = 1000, height = 1000, unit = "px")
barplot(dtm_d[1:9,]$freq, las = 2, names.arg = dtm_d[1:9,]$word,col ="lightgreen", main ="Top 8 most frequent words",ylab = "Word frequencies")


####################################################################
#generate word cloud
set.seed(1234)
dev.new(width = 1000, height = 1000, unit = "px")
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
          max.words=300, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))
####################################################################
# Find associations 
#findAssocs(TextDoc_dtm, terms = c("أکآ®أ™آˆأکآ¨","أکآ¨أکآ¯","أکآ²أ›آŒأکآ§أکآ¯"), corlimit = 0.25)

######################################################################
# Find associations for words that occur at least 50 times
#findAssocs(TextDoc_dtm, terms = findFreqTerms(TextDoc_dtm, lowfreq = 50), corlimit = 0.25)


#####################################################################################
##########################sentment analisis########################
####################################################################################



if(!require("quanteda")) {install.packages("quanteda"); library("quanteda")}
if(!require("tidyverse")) {install.packages("tidyverse"); library("tidyverse")}
if(!require("googleLanguageR")) {install.packages("googleLanguageR"); library("googleLanguageR")}
theme_set(theme_bw())

data<-read.csv("C:\\Users\\98939\\Desktop\\navad_eghtesadi.csv",encoding = "UTF-8")
text<-data$tweet




##############################################################################
lex<-read.csv("C:\\Users\\98939\\Desktop\\text mining\\final_lexicon_without_duplicated_values_and_zeros.csv",encoding = "UTF-8")

library(dplyr)
positive.words<-filter(lex ,sentiment==1)$Persian.Translation..Google.Translate.
negative.words<-filter(lex ,sentiment==-1)$Persian.Translation..Google.Translate.

#positive.words[positive.words %in% positive.words.bl]


positive.words.bl <- scan("C:\\Users\\98939\\Desktop\\text mining\\lexicon fa\\positive.txt", what = "char", sep = "\n", quiet = T,encoding = "UTF-8")
negative.words.bl <- scan("C:\\Users\\98939\\Desktop\\text mining\\lexicon fa\\negative.txt", what = "char", sep = "\n",  quiet = T,encoding = "UTF-8")

positive.words.bl <- unique(c(positive.words,positive.words.bl))
negative.words.bl  <- unique(c(negative.words,negative.words.bl))



sentiment.dict.fa <- dictionary(list(positive = positive.words.bl, negative = negative.words.bl))



dfm.sentiment.germandict <- 
  text %>% 
  dfm(dictionary = sentiment.dict.fa) %>% 
  dfm_weight(scheme = "prop")
dfm.sentiment.germandict

View(dfm.sentiment.germandict)

dfm.sentiment.germandict<-as.data.frame(dfm.sentiment.germandict)

for (i in 1:dim(dfm.sentiment.germandict)[1]){
  
  if (dfm.sentiment.germandict[i,2]>0.50){dfm.sentiment.germandict[i,4]<-1}
  else {dfm.sentiment.germandict[i,4]<-0}
  
}
dfm.sentiment.germandict






data_new<-data.frame(date=data$date,sentiment=dfm.sentiment.germandict$V4)

str(data_new)

data_new$date <- as.Date(data_new$date)

data_final<-aggregate(data_new["sentiment"], by=data_new["date"], mean)



########################################################


#data_final[,1] <- as.Date(as.character(data_final[,1]), "%Y%m%d")

start = data_final$date[1]
end=data_final$date[length(data_final$date)]

#length(seq(from=start, to=end, by='day')) - 1

full <- seq(from=start, to=end, by='day')
full



with(data_final, data_final$sentiment[match(full,data_final[,1])])

data_final_new<-data.frame(Date=full,sentiment=with(data_final, data_final$sentiment[match(full,data_final[,1])]))
data_final_new


data_final_new$sentiment[is.na(data_final_new$sentiment)] <- 0

data_final_new
#######################################################
saveRDS(data_final_new,"C:\\Users\\98939\\Desktop\\navad_eghtesadi.RData")

