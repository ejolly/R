#Code for analyzing twitter data and visualizing basic descriptive information
#The final visualization used this code along with R markdown to generate an
#html file
#Eshin Jolly

#The Sentiment Lexicon used for this analysis came from:
#http://www.cs.uic.edu/~liub/FBS/opinion-lexicon-English.rar
#Reference:
#Bing Liu, Minqing Hu and Junsheng Cheng. "Opinion Observer: Analyzing
# and Comparing Opinions on the Web." Proceedings of the 14th
# International World Wide Web conference (WWW-2005), May 10-14,
# 2005, Chiba, Japan


##### GENERAL SETUP ####
library('twitteR') #for pulling tweets and setting up access
library('ROAuth') #Don't think I ended up using this
library('tm') #text mining package
library('wordcloud') #visualizing wordclouds
library('stringr') #parsing strings in tweets
library('corrplot') #awesome for plotting correlation matrices
library('ggplot2')
library('plyr')

#Load in Sentiment Lexicon
pos.words <- read.table(file.path(getwd(),'forFun','positive_words.txt'))
neg.words <- read.table(file.path(getwd(),'forFun','negative_words.txt'))

#Functions to further clean tweets
removeEmoticons <- function(x) {iconv(x, "latin1", "ASCII", sub="")}
removeURLs <- function(x) {gsub("http[[:alnum:]]*","",x)}


#Function to perform sentiment analysis with sentiment lexicon
#Modifed code from:
#https://github.com/jeffreybreen/twitter-sentiment-analysis-tutorial-201107/blob/master/R/sentiment.R
score.sentiment <- function(sentences,pos.words,neg.words) {
  scores <- laply(sentences, function(sentence,
                                      pos.words,
                                      neg.words) {
    # split into words, ignoring new line characters
    words <- unlist(str_split(sentence, '\\s+'))
    # compare words to the dictionaries of positive & negative terms
    pos.matches <- !is.na(match(words,pos.words$V1))
    neg.matches <- !is.na(match(words, neg.words$V1))
    # generate scores
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words)
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}

#Function to compute pairwise correlation matrix of terms matching a specific
#frequency
#This is pretty ineffeciently coded and takes long if more words
#(i.e. lower frequency words) are used.
#Requires a Term Document Matrix as input (see tm package)
cor.tdm <- function(n,tdm){

  require('tm')
  top <- findFreqTerms(tdm,lowfreq=n)

  mat <- matrix(NA,length(top),length(top))

  for (i in 1:length(top)){
    f <- findAssocs(tdm,
                    tdm$dimnames$Terms[tdm$dimnames$Terms==top[i]],
                    corlimit=.01)
    mat[i,i] <- 1
    t <- setdiff(top,top[i])
    tidx <- which(top %in% top[-i])

    for (j in 1:length(t)){
      idx <- which(rownames(f) %in% t[j])
      mat[i,tidx[j]] <- ifelse(length(idx==1),f[idx],0)
    }
  }
  rownames(mat) <- c(top)
  colnames(mat) <- c(top)
  return (mat)
}

#Authenticate twitter access
#This is a bit easier that using ROAuth and can cache your authorization
#for future use
setup_twitter_oauth(consumer_key = 'XXXXXXXX',
                    consumer_secret= 'XXXXXX',
                    access_token = 'XXXXXXX',
                    access_secret = 'XXXXXXX'
)

#Colors for plotting
cols <- colorRampPalette(c('royalblue4', 'firebrick1', "yellow"))


##### Collect tweets, clean them and build dataframes for plotting ####

#Collect 10,000 Tweets
tweet <- searchTwitter('#InternationalWomensDay', n = 10000)
#Convert to dataframe
twDF <- twListToDF(tweet)

#It's a good idea to save and load the tweets and dataframe for future use
#since they are pretty large
save(tweet,file = file.path(getwd(),'forFun','Twitter','10000_tweets.Rdata'))
save(twDF,file = file.path(getwd(),'forFun','Twitter','tweetsDF.Rdata'))

#For future use
#load(file.path(getwd(),'forFun','10000_tweets.Rdata'))
#load(file.path(getwd(),'forFun','tweetsDF.Rdata'))

#Clean them up using corpus methods from tm
tweets <- Corpus(VectorSource(twDF$text))
tweets <- tm_map(tweets, content_transformer(removeEmoticons))
tweets <- tm_map(tweets, content_transformer(tolower))
tweets <- tm_map(tweets, removePunctuation)
tweets <- tm_map(tweets, removeNumbers)
tweets <- tm_map(tweets, content_transformer(removeURLs))
#Remove the hashtag and the retweet abbreviation in addition to common stop words
tweets <- tm_map(tweets, removeWords,c(stopwords('english'),'rt','internationalwomensday'))

#Convert back to a dataframe
tweetsDF<-data.frame(text=unlist(sapply(tweets, `[`, "content")),
                     stringsAsFactors=F)


#Now build a Document Matrix
tdm <- DocumentTermMatrix(tweets)

#Save this too
save(tdm,file = file.path(getwd(),'forFun','Twitter','documentTermMat.Rdata'))

#Get word counts
countsDF <- data.frame(count = colSums(as.matrix(tdm)),
                       word = colnames(as.matrix(tdm)))

#Terms associated with "women"
womenAssoc <- findAssocs(tdm,'women',corlimit=.2)
womenAssoc <- as.data.frame(womenAssoc)
womenAssoc$word <- rownames(womenAssoc)

#Save it
save(countsDF,file = file.path(getwd(),'forFun','Twitter','tweetCounts.Rdata'))

#Build a correlation matrix of words mentioned in at least 200 tweets
#WARNING: this takes a while, because of how inefficently I setup the loop
corMat <- cor.tdm(200,tdm)

#Save it
save(corMat,file = file.path(getwd(),'forFun','Twitter','tweetsCorr.Rdata'))

#Sentiment analysis (could definitely be cleaner and less redundant)
#Compute sentiment on all tweets by comparing to Sentiment Lexicon
sentDF <- score.sentiment(tweetsDF$text,pos.words,neg.words)
sentDF$score <- factor(sentDF2$score)

#Save this for future use
save(sentDF,file = file.path(getwd(),'forFun','Twitter','sentimentDF.Rdata'))

#Make a new dataframe of proportions for plotting
sentDFs <- data.frame(percent = sapply(sort(unique(sentDF$score)),function(x){sum(sentDF$score==x)/nrow(sentDF)}), score= sort(unique(sentDF$score)))
sentDFs$score <- as.numeric(levels(sentDFs$score))[sentDFs$score]
sentDFs <- rbind(c(0,-6),c(0,-5),sentDFs)
sentDFs$score <- as.factor(sentDFs$score)
mscore <- mean(as.numeric(levels(sentDF$score)[sentDF$score]))

#### Visualize stuff ####

#Wordcloud of all Tweeted words
wordcloud(countsDF$word, countsDF$count, min.freq=10, scale=c(7,.5),colors =cols(20),
          random.color= F, random.order = F, rot.per=.3, max.words = 1000)


#Terms associated with women
ggplot(womenAssoc,aes(reorder(word,women), women,fill=word)) + geom_histogram(stat='identity') +
  theme_bw() +
  labs(x ='',y='Correlation strength') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=11),
        axis.ticks.x=element_blank(), axis.title.y = element_text(size=13,vjust=1.5)) +
  guides(fill=F) +
  coord_cartesian(ylim=c(.18,.31)) +
  ggtitle('Words that most often co-occur with "Women"')


#Correlation matrix with hierarchical clustering
corrplot((corMat2), is.corr = F, method= "circle", order = 'hclust',hclust.method='ward',
         col= cols(30), cl.lim = c(0,1),
         tl.cex = .75, tl.col = 'black', tl.srt = 45, tl.offset= .5,
         cl.ratio=.2, cl.cex = .6, cl.offset=20,
         outline = F, addrect=6, rect.col='royalblue4',rect.lwd = 2)


#Sentiment plot
ggplot(sentDFs,aes(x = score, y = percent, fill=score)) + geom_bar(stat='identity') +
  theme_bw() +
  labs(x='',y='Proportion of 10,000 Tweets') +
  guides(fill=F) +
  theme(axis.text.x = element_text(size=12),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size=12, vjust=1.5)) +
  geom_vline(xintercept=7+mscore,size = 1.25,linetype='longdash',colour='goldenrod') +
  ggtitle('Emotional Sentiment Among Tweets') +
  coord_cartesian(ylim=c(0,.5)) +
  scale_x_discrete(breaks = -6:6,labels=c('Very Negative',
                                          '','','','','',
                                          'Neutral',
                                          '','', '', '', '',
                                          'Very Positive'))

