# TwitterSentiment

The following script [TwittAnlysis.R](https://github.com/MarcusoHanlon/TwitterSentiment/blob/master/TwittAnalysis.R) allows user to connect to the Twitter Api, pull a spre-selected number of tweets, plot these tweets in a word cloud and then analyse the sentiment associated with the tweets.

The analysis was carried out to assess the sentiment associated with Conor McGregor following an interview he had with Aerial Helawi on the 5th of October 2015.

The sentiment is analysed using a [Naive Bayes bag-of-words Method](https://en.wikipedia.org/wiki/Bag-of-words_model). The method uses and orderless document to represent the frequencies of words from a dictionary. And then uses an naive Bayes classifier to predicit the wether the tweet carries negative or positive connotations.

<img src="http://www.saedsayad.com/images/Bayes_rule.png" align="Centre"/>

#Process
1)Connect to the Twitter API
2)Collect required Tweets (search term + number of tweets)
3)Create term frequency document
4)Use Naive Bayes bag-of-words classifier to classify Sentiment + Emotion of Tweet
5)Visualize

#Further reading
[Comparison of text clasification algorithms](http://www.inf.ed.ac.uk/teaching/courses/inf2b/learnnotes/inf2b-learn-note07-2up.pdf)

#WordCloud
<img src="https://raw.githubusercontent.com/MarcusoHanlon/TwitterSentiment/master/MachineLearningCloud.png" align="Left"/>
#Associated Sentiment
<img src="https://raw.githubusercontent.com/MarcusoHanlon/TwitterSentiment/master/Sentiment.png" align="middle"/>
#Emotions Conveyed
<img src="https://raw.githubusercontent.com/MarcusoHanlon/TwitterSentiment/master/Emotions.png" align="middle"/>
#Words/Tweet
<img src="https://raw.githubusercontent.com/MarcusoHanlon/TwitterSentiment/master/words.png" align="middle"/>
