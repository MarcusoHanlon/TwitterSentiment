library(twitteR)
library(plyr)
library(ROAuth)
library(tm)
library(wordcloud)
library(RColorBrewer)
#library(sentiment)
library(stringr)
library(ggplot2)

Cust_Key = 'yUQxhTuLxIOIzKyrxu0eT0Qll'
Cust_Secret =  'ySMgzDcOET6huwBlbW2ryNI3WnSWrmf1SUCriz96ImcVMTshUa'
Access_Token = '725803508-bokr4EtshvF6lS69OYHdRVPkXxk3GTeRH5xbdpjv'
Access_Secret = '41rGSpQBC4W0BioWHcNWhIjCM76jG5T2xVVi6xKHtxaBQ'

###########################################
#####Setup the Twitter Connection##########
###########################################
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
cred <- OAuthFactory$new(consumerKey='yUQxhTuLxIOIzKyrxu0eT0Qll',consumerSecret='ySMgzDcOET6huwBlbW2ryNI3WnSWrmf1SUCriz96ImcVMTshUa',requestURL='https://api.twitter.com/oauth/request_token',accessURL='https://api.twitter.com/oauth/access_token',authURL='https://api.twitter.com/oauth/authorize')
#necessary step for Windows
#cred$handshake(cainfo="cacert.pem")
#save for later use for Windows
save(cred, file="twitter authentication.Rdata")

setup_twitter_oauth(Cust_Key, Cust_Secret, Access_Token, Access_Secret)

############################################
##########RetrieveTweets####################
############################################
tweets = searchTwitter("TheNotoriousMMA", n=200,lang="en")
mach_text = laply(tweets,function(t)t$getText())
View(mach_text)

View(bjp_txt)
############################################
#########Create the WordCloud#################
############################################

mach_corpus = Corpus(VectorSource(bjp_txt))
tdm = TermDocumentMatrix(mach_corpus,
   control = list(removePunctuation = TRUE,
   stopwords = c("machine", "learning", stopwords("english")),
   removeNumbers = TRUE, tolower = TRUE))
View(tdm)

m = as.matrix(tdm)
View(m)

# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE) 
# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)
View(word_freqs)
# plot wordcloud
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

# save the image in png format
png("MachineLearningCloud.png", width=12, height=8, units="in", res=300)
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

###################################################
############Analyse the Senitment##################
###################################################

bjp_txt = sapply(tweets,function(x)x$getText())

#First we will remove retweet entities from the stored tweets (text)
bjp_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", bjp_txt)
# Then remove all “@people”
bjp_txt = gsub("@\\w+", " ", bjp_txt)
# Then remove all the punctuation
bjp_txt = gsub("[[:punct:]]", " ", bjp_txt)
# Then remove numbers, we need only text for analytics
bjp_txt = gsub("[[:digit:]]", " ", bjp_txt)
# the remove html links, which are not required for sentiment analysis
bjp_txt = gsub("http\\w+", "", bjp_txt)
# finally, we remove unnecessary spaces (white spaces, tabs etc)
bjp_txt = gsub("[ \t]{2,}", " ", bjp_txt)
bjp_txt = gsub("^\\s+|\\s+$", " ", bjp_txt)

catch.error = function(x)
{
# let us create a missing value for test purpose
y = NA
# try to catch that error (NA) we just created
catch_error = tryCatch(tolower(x), error=function(e) e)
# if not an error
if (!inherits(catch_error, "error"))
y = tolower(x)
# check result if error exists, otherwise the function works fine.
return(y)
}

bjp_txt = sapply(bjp_txt, catch.error)


