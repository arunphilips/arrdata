#R Script for Twitter Mining to find out consumer sentiment about an airline

install.packages("twitteR") #Installing twitteR package (read JSON)
library(twitteR) #loading package

install.packages(c("devtools", "rjson", "bit64", "httr"))
library(httr)

install.packages("base64enc")
library(base64enc)

install.packages("plyr")
library(plyr)

#Setting up Twitter Authentication
api_key <- ""

api_secret <- ""

access_token <- ""

access_token_secret <- ""

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

#Obtaining 1500 tweets that mention jetairways
jet.tweets = searchTwitter('@jetairways', n=1500) #Return a list of tweets

firstel <- jet.tweets[[1]] 

class(firstel) #Object of type status, from twitteR package

firstel$getText() #Get text of the tweet

#get text from each tweet into an array
jet.text = laply(jet.tweets, function(x) x$getText() )
head(jet.text)

#Obtaining 1500 tweets that mention Indigo
indigo.tweets = searchTwitter('@IndiGo6E', n=1500) 

#get text from each tweet into an array
indigo.text = laply(indigo.tweets, function(x) x$getText() )
head(indigo.text)

#Obtaining 1500 tweets that mention Spicejet
spice.tweets = searchTwitter('@flyspicejet', n=1500) 

#get text from each tweet into an array
spice.text = laply(spice.tweets, function(x) x$getText() )
head(spice.text)

#Obtaining 1500 tweets that mention Vistara
vistara.tweets = searchTwitter('@airvistara', n=1500) 

#get text from each tweet into an array
vistara.text = laply(vistara.tweets, function(x) x$getText() )
head(vistara.text)

#Getting positive and negative word list
positive = scan("positive-words.txt", what='character', comment.char=';')
negative = scan("negative-words.txt", what='character', comment.char=';')

#Adding some possible positive and negative words
pos.words = c(positive, 'upgrade', 'kudos', 'lounge')
neg.words = c(negative,  'wtf', 'wait', 'waiting','epicfail', 'mechanical', 'indiathings')


#Sentiment Scoring Function
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
        require(plyr)
        require(stringr)
        
        # we got a vector of sentences. plyr will handle a list
        # or a vector as an "l" for us
        # we want a simple array of scores back, so we use
        # "l" + "a" + "ply" = "laply":
        scores = laply(sentences, function(sentence, pos.words, neg.words) {
                
                # clean up sentences with R's regex-driven global substitute, gsub():
                sentence = gsub('[[:punct:]]', '', sentence)
                sentence = gsub('[[:cntrl:]]', '', sentence)
                sentence = gsub('\\d+', '', sentence)
                # and convert to lower case:
                sentence = tolower(sentence)
                
                # split into words. str_split is in the stringr package
                word.list = str_split(sentence, '\\s+')
                # sometimes a list() is one level of hierarchy too much
                words = unlist(word.list)
                
                # compare our words to the dictionaries of positive & negative terms
                pos.matches = match(words, pos.words)
                neg.matches = match(words, neg.words)
                
                # match() returns the position of the matched term or NA
                # we just want a TRUE/FALSE:
                pos.matches = !is.na(pos.matches)
                neg.matches = !is.na(neg.matches)
                
                # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
                score = sum(pos.matches) - sum(neg.matches)
                
                return(score)
        }, pos.words, neg.words, .progress=.progress )
        
        scores.df = data.frame(score=scores, text=sentences)
        return(scores.df)
}

#Storing the consumer scores of each airline
jetresult = score.sentiment(jet.text, pos.words, neg.words)

indigoresult = score.sentiment(indigo.text, pos.words, neg.words)

spiceresult = score.sentiment(spice.text, pos.words, neg.words)

vistararesult = score.sentiment(vistara.text, pos.words, neg.words)

#Sum of individual tweet scores
JetAirways<-sum(jetresult$score)
Indigo<-sum(indigoresult$score)
SpiceJet<-sum(spiceresult$score)
Vistara<-sum(vistararesult$score)

scorearray = c(Indigo, SpiceJet, JetAirways, Vistara)

#Plotting a barplot with scores
b<-barplot(scorearray, names.arg = c("Indigo", "SpiceJet", "Jet Airways", "Vistara"),
        main="Consumer Airline Sentiments - 1st June 2016", col=c("green", "yellow", "blue", "red"),
        xlab="Airlines", ylab="Consumer Score", beside =TRUE, sub="Data from Twitter, https://github.com/arunphilips/arrdata/")

text(cex=.5, x=b, y=scorearray+par("cxy")[2]/2, round(scorearray,2), xpd=TRUE)
#Adding values of bar plots

