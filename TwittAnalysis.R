library(twitteR)
library(plyr)
library(ROAuth)
library(tm)
library(wordcloud)
library(RColorBrewer)
#library(sentiment)
library(stringr)
library(ggplot2)
library(RTextTools)

getwd()

Cust_Key = 'geVsNXcQTGerWBsBfKMw2YtBk'
Cust_Secret =  'KV7PeGQdwYk5fcyaGNcmQHeuEpxDG8Zrz4Cf8miyuRmSHI2cTB'
Access_Token = '725803508-bokr4EtshvF6lS69OYHdRVPkXxk3GTeRH5xbdpjv'
Access_Secret = '41rGSpQBC4W0BioWHcNWhIjCM76jG5T2xVVi6xKHtxaBQ'

###########################################
#####Setup the Twitter Connection##########
###########################################
setup_twitter_oauth(Cust_Key, Cust_Secret, access_token=NULL, access_secret=NULL)

############################################
##########RetrieveTweets####################
############################################
tweets = searchTwitter("TheNotoriousMMA", n=200,lang="en")
mach_text = laply(tweets,function(t)t$getText())

############################################
###########Clean Tweets#####################
############################################

bjp_txt = sapply(tweets,function(x)x$getText())

clean_tweet = gsub("&amp", "",bjp_txt)
clean_tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", clean_tweet)
clean_tweet = gsub("[[:punct:]]", " ", clean_tweet)
clean_tweet = gsub("[[:digit:]]", " ", clean_tweet)
clean_tweet = gsub("http\\w+", "", clean_tweet)
clean_tweet = gsub("[ \t]{2,}", " ", clean_tweet)
clean_tweet = gsub("^\\s+|\\s+$", " ", clean_tweet)
clean_tweet = gsub("http", "", clean_tweet)

# Then remove all “@people”
#clean_tweet = gsub("@\\w+", " ", bjp_txt
View(clean_tweet)
############################################
#########Create the WordCloud Data##########
############################################

mach_corpus = Corpus(VectorSource(clean_tweet))
tdm = TermDocumentMatrix(mach_corpus,
   control = list(removePunctuation = TRUE,
   stopwords = c("machine", "learning", stopwords("english")),
   removeNumbers = TRUE, tolower = TRUE))

m = as.matrix(tdm)

# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE) 
# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)
View(word_freqs)

###########################
#####plot wordcloud########
###########################
wc <- wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

# save the image in png format

png("MachineLearningCloud.png", width=12, height=8, units="in", res=300)
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

###################################################
############Classify Emotion################
###################################################
classify_emotion <- function(textColumns,algorithm="bayes",prior=1.0,verbose=FALSE,...) {
	matrix <- create_matrix(textColumns,...)
	lexicon <- read.csv(file='C:/Users/Aron/Documents/GitHub/TwitterSentiment/emotions.csv',header=FALSE)

	counts <- list(anger=length(which(lexicon[,2]=="anger")),disgust=length(which(lexicon[,2]=="disgust")),fear=length(which(lexicon[,2]=="fear")),joy=length(which(lexicon[,2]=="joy")),sadness=length(which(lexicon[,2]=="sadness")),surprise=length(which(lexicon[,2]=="surprise")),total=nrow(lexicon))
	documents <- c()

	for (i in 1:nrow(matrix)) {
		if (verbose) print(paste("DOCUMENT",i))
		scores <- list(anger=0,disgust=0,fear=0,joy=0,sadness=0,surprise=0)
		doc <- matrix[i,]
		words <- findFreqTerms(doc,lowfreq=1)
		
		for (word in words) {
            for (key in names(scores)) {
                emotions <- lexicon[which(lexicon[,2]==key),]
                index <- pmatch(word,emotions[,1],nomatch=0)
                if (index > 0) {
                    entry <- emotions[index,]
                    
                    category <- as.character(entry[[2]])
                    count <- counts[[category]]
        
                    score <- 1.0
                    if (algorithm=="bayes") score <- abs(log(score*prior/count))
            
                    if (verbose) {
                        print(paste("WORD:",word,"CAT:",category,"SCORE:",score))
                    }
                    
                    scores[[category]] <- scores[[category]]+score
                }
            }
        }
        
        if (algorithm=="bayes") {
            for (key in names(scores)) {
                count <- counts[[key]]
                total <- counts[["total"]]
                score <- abs(log(count/total))
                scores[[key]] <- scores[[key]]+score
            }
        } else {
            for (key in names(scores)) {
                scores[[key]] <- scores[[key]]+0.000001
            }
        }
		
        best_fit <- names(scores)[which.max(unlist(scores))]
        if (best_fit == "disgust" && as.numeric(unlist(scores[2]))-3.09234 < .01) best_fit <- NA
		documents <- rbind(documents,c(scores$anger,scores$disgust,scores$fear,scores$joy,scores$sadness,scores$surprise,best_fit))
	}
	
	colnames(documents) <- c("ANGER","DISGUST","FEAR","JOY","SADNESS","SURPRISE","BEST_FIT")
	return(documents)
}


##################################
####Classify Polarity#############
##################################

classify_polarity <- function(textColumns,algorithm="bayes",pstrong=0.5,pweak=1.0,prior=1.0,verbose=FALSE,...) {
	matrix <- create_matrix(textColumns,...)
	lexicon <- read.csv(file='C:/Users/Aron/Documents/GitHub/TwitterSentiment/subjectivity.csv',header=FALSE)


	counts <- list(positive=length(which(lexicon[,3]=="positive")),negative=length(which(lexicon[,3]=="negative")),total=nrow(lexicon))
	documents <- c()

	for (i in 1:nrow(matrix)) {
		if (verbose) print(paste("DOCUMENT",i))
		scores <- list(positive=0,negative=0)
		doc <- matrix[i,]
		words <- findFreqTerms(doc,lowfreq=1)
		
		for (word in words) {
			index <- pmatch(word,lexicon[,1],nomatch=0)
			if (index > 0) {
				entry <- lexicon[index,]
				
				polarity <- as.character(entry[[2]])
				category <- as.character(entry[[3]])
				count <- counts[[category]]
	
				score <- pweak
                if (polarity == "strongsubj") score <- pstrong
				if (algorithm=="bayes") score <- abs(log(score*prior/count))
		
				if (verbose) {
                    print(paste("WORD:",word,"CAT:",category,"POL:",polarity,"SCORE:",score))
				}
				
				scores[[category]] <- scores[[category]]+score
			}		
		}
		
		if (algorithm=="bayes") {
			for (key in names(scores)) {
				count <- counts[[key]]
				total <- counts[["total"]]
				score <- abs(log(count/total))
				scores[[key]] <- scores[[key]]+score
			}
		} else {
			for (key in names(scores)) {
				scores[[key]] <- scores[[key]]+0.000001
			}
		}
		
        best_fit <- names(scores)[which.max(unlist(scores))]
        ratio <- as.integer(abs(scores$positive/scores$negative))
        if (ratio==1) best_fit <- "neutral"
		documents <- rbind(documents,c(scores$positive,scores$negative,abs(scores$positive/scores$negative),best_fit))
		if (verbose) {
			print(paste("POS:",scores$positive,"NEG:",scores$negative,"RATIO:",abs(scores$positive/scores$negative)))
			cat("\n")
		}
	}
	
	colnames(documents) <- c("POS","NEG","POS/NEG","BEST_FIT")
	return(documents)
}


##################################
####Use the Function##############
##################################

bjp_class_emo = classify_emotion(clean_tweet, algorithm="bayes", prior=1.0)
emotion = bjp_class_emo[,7] ##bestfit
emotion[is.na(emotion)] = "unknown"

bjp_class_pol = classify_polarity(clean_tweet, algorithm="bayes")
polarity = bjp_class_pol[,4] ##bestfit

###################################
#######Plot The Sentiment Analysis########################
###################################
sentiment_dataframe = data.frame(text=clean_tweet, emotion=emotion, polarity=polarity, stringsAsFactors=FALSE)
sentiment_dataframe = within(sentiment_dataframe, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
View(sentiment_dataframe)

ggp <- ggplot(sentiment_dataframe, aes(x=emotion)) + geom_bar(aes(y=..count.., fill=emotion)) +
scale_fill_brewer(palette="Dark2") +
ggtitle('Emotions Conveyed in Tweets on Twitter about Conor McGregor') +
theme(legend.position='right') + ylab('Number of Tweets') + xlab('Emotion Categories')

ggp2 <- ggplot(sentiment_dataframe, aes(x=polarity)) +
geom_bar(aes(y=..count.., fill=polarity)) +
scale_fill_brewer(palette="RdGy") +
ggtitle('Sentiment Analysis of Tweets on Twitter about Conor McGregor') +
theme(legend.position='right') + ylab('Number of Tweets') + xlab('Polarity Categories')

ggsave("Emotions.png",ggp,width=14,height=10,units="in")
ggsave("Sentiment.png",ggp2,width=14,height=10,units="in")

#################################
#######Further Analysis##########
#################################

chars_per_tweet = sapply(clean_tweet, nchar)

# split words
words_list = strsplit(clean_tweet, " ")
# words per tweet
words_per_tweet = sapply(words_list, length)
# barplot
ggp3<-barplot(table(words_per_tweet), border=NA,
   main="Distribution of words per tweet", cex.main=1)

ggsave("Words.png",ggp3,width=14,height=10,units="in")
