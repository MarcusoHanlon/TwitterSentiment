library(twitteR)
library(plyr)
library(ROAuth)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(sentiment)
library(stringr)
library(ggplot2)




###Key iQWflxiOgeJmFoj4tAezllybX
###Secret 0ywdjknLjMY9H3ssMgEcS1fZjnm0QXL9VGfrcCYt625lDIjyON

###########################################
#####Setup the Twitter Connection##########
###########################################

####################################################################
#####More details found here: http://scn.sap.com/docs/DOC-53354######
####################################################################

download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
cred <- OAuthFactory$new(consumerKey='iQWflxiOgeJmFoj4tAezllybX',consumerSecret='0ywdjknLjMY9H3ssMgEcS1fZjnm0QXL9VGfrcCYt625lDIjyON',requestURL='https://api.twitter.com/oauth/request_token',accessURL='https://api.twitter.com/oauth/access_token',authURL='https://api.twitter.com/oauth/authorize')
#necessary step for Windows
cred$handshake(cainfo="cacert.pem")
#save for later use for Windows
save(cred, file="twitter authentication.Rdata")
registerTwitterOAuth(cred)

############################################
##########RetrieveTweets####################
############################################
tweets = searchTwitter("Accenture", n=200,lang="en", cainfo="cacert.pem")
mach_text = laply(tweets,function(t)t$getText())
View(mach_text)

############################################
#########Create the WordCloud#################
############################################

mach_corpus = Corpus(VectorSource(mach_text))
tdm = TermDocumentMatrix(mach_corpus,
   control = list(removePunctuation = TRUE,
   stopwords = c("machine", "learning", stopwords("english")),
   removeNumbers = TRUE, tolower = TRUE))


m = as.matrix(tdm)


# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE) 
# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)

# plot wordcloud
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

# save the image in png format
png("MachineLearningCloud.png", width=12, height=8, units="in", res=300)
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

###################################################
############Clean The Data##################
###################################################

bjp_txt = sapply(tweets,function(x)x$getText())

#First we will remove retweet entities from the stored tweets (text)
bjp_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", bjp_txt)
# Then remove all “@people”
bjp_txt = gsub("@\\w+", "", bjp_txt)
# Then remove all the punctuation
bjp_txt = gsub("[[:punct:]]", "", bjp_txt)
# Then remove numbers, we need only text for analytics
bjp_txt = gsub("[[:digit:]]", "", bjp_txt)
# the remove html links, which are not required for sentiment analysis
bjp_txt = gsub("http\\w+", "", bjp_txt)
# finally, we remove unnecessary spaces (white spaces, tabs etc)
bjp_txt = gsub("[ \t]{2,}", "", bjp_txt)
bjp_txt = gsub("^\\s+|\\s+$", "", bjp_txt)

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

bjp_txt = bjp_txt[!is.na(bjp_txt)]

names(bjp_txt) = NULL



###################################################
############Create Matrix##########################
###################################################
create_matrix <- function(textColumns, language="english", minDocFreq=1, minWordLength=3, removeNumbers=TRUE, removePunctuation=TRUE, removeSparseTerms=0, removeStopwords=TRUE, stemWords=FALSE, stripWhitespace=TRUE, toLower=TRUE, weighting=weightTf) {
	
    stem_words <- function(x) {
        split <- strsplit(x," ")
        return(wordStem(split[[1]],language=language))
    }
	
	control <- list(language=language,tolower=toLower,removeNumbers=removeNumbers,removePunctuation=removePunctuation,stripWhitespace=stripWhitespace,minWordLength=minWordLength,stopwords=removeStopwords,minDocFreq=minDocFreq,weighting=weighting)
    
    if (stemWords == TRUE) control <- append(control,list(stemming=stem_words),after=6)
    
    trainingColumn <- apply(as.matrix(textColumns),1,paste,collapse=" ")
    trainingColumn <- sapply(as.vector(trainingColumn,mode="character"),iconv,to="UTF8",sub="byte")

	corpus <- Corpus(VectorSource(trainingColumn),readerControl=list(language=language))
	matrix <- DocumentTermMatrix(corpus,control=control);
    if (removeSparseTerms > 0) matrix <- removeSparseTerms(matrix,removeSparseTerms)
	
	gc()
	return(matrix)
}

####################################################
##########Classify Emotions#########################
####################################################

classify_emotion <- function(textColumns,algorithm="bayes",prior=1.0,verbose=FALSE,...) {
	matrix <- create_matrix(textColumns,...)
	lexicon <- read.csv(file='C:/R/Twitter/emotions.csv',header=FALSE)

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
	lexicon <- read.csv(file='C:/R/Twitter/subjectivity.csv',header=FALSE)


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

bjp_class_emo = classify_emotion(bjp_txt, algorithm="bayes", prior=1.0)
emotion = bjp_class_emo[,7] ##bestfit
emotion[is.na(emotion)] = "unknown"

bjp_class_pol = classify_polarity(bjp_txt, algorithm="bayes")
polarity = bjp_class_pol[,4] ##bestfit

###################################
#######Plot The Sentiment Analysis########################
###################################
sentiment_dataframe = data.frame(text=bjp_txt, emotion=emotion, polarity=polarity, stringsAsFactors=FALSE)
sentiment_dataframe = within(sentiment_dataframe, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
View(sentiment_dataframe)

ggplot(sentiment_dataframe, aes(x=emotion)) + geom_bar(aes(y=..count.., fill=emotion)) +
scale_fill_brewer(palette="Dark2") +
ggtitle('Emotions Conveyed in Tweets on Twitter about Accenture') +
theme(legend.position='right') + ylab('Number of Tweets') + xlab('Emotion Categories')

ggplot(sentiment_dataframe, aes(x=polarity)) +
geom_bar(aes(y=..count.., fill=polarity)) +
scale_fill_brewer(palette="RdGy") +
ggtitle('Sentiment Analysis of Tweets on Twitter about Accenture') +
theme(legend.position='right') + ylab('Number of Tweets') + xlab('Polarity Categories')

#################################
#######Further Analysis##########
#################################

chars_per_tweet = sapply(bjp_txt, nchar)

# split words
words_list = strsplit(bjp_txt, " ")
# words per tweet
words_per_tweet = sapply(words_list, length)
# barplot
barplot(table(words_per_tweet), border=NA,
   main="Distribution of words per tweet", cex.main=1)

# most frequent words
mfw = sort(table(unlist(words_list)), decreasing=TRUE)

# top-20 most frequent
top20 = head(mfw, 20)

# barplot
barplot(top20, border=NA, las=2, main="Top 20 most frequent terms", cex.main=1)