textData <- read.csv("tweets.csv",stringsAsFactors=FALSE)
str(textData)
textData1<-readLines("tweets.txt")
str(textData1)



# Creating object with the URL
conURL <- "http://en.wikipedia.org/wiki/R_%28programming_language%29"

# Establish the connection with the URL
link2URL <- url(conURL)

# Reading html code
htmlCode <- readLines(link2URL)

# Closing the connection
close(link2URL)

# Printing the result
htmlCode


# Extracting first observation
text2process <- textData$TWEET[1]
text2process


prepRemovedText <- gsub(pattern="for",replacement="",x=text2process)
prepRemovedText

splittedText <- strsplit(text2process,split=" ")
splittedText
unlist(splittedText)
tolower(text2process)
toupper(text2process)
nchar(text2process)
nchar(unlist(splittedText))


# Creating the character string with date and time information
dateTimeobject <- "02Feb2015:11:15PM"
# Extracting only the character between 1 to 9
# including 1st and 9th
substr(dateTimeobject,1,9)


# to see the color names
colors()
# Now to extract the digit from the color names
as.integer(gsub("\\D", "", colors()))


# Readers require to provide their key in the following code.
library(twitteR)
setup_twitter_oauth(consumer_key="xxxxxx",
                    consumer_secret="xxxxxxx",
                    access_token="xxxxxx",
                    access_secret="xxxxxxx")

tweets<-searchTwitter("#rstats", n=500,lang='en')


datTweet<-plyr::ldply(tweets,as.data.frame)
vecStatus <- datTweet$text

#Clean Text
vecStatus = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",vecStatus)
vecStatus = gsub("http[^[:blank:]]+", "", vecStatus)
vecStatus = gsub("@\\w+", "", vecStatus)
vecStatus = gsub("[ \t]{2,}", "", vecStatus)
vecStatus = gsub("^\\s+|\\s+$", "", vecStatus)
vecStatus <- gsub('\\d+', '', vecStatus)
vecStatus = gsub("[[:punct:]]", " ", vecStatus)

library(tm)
library(wordcloud)


corpus = Corpus(VectorSource(vecStatus))
corpus = tm_map(corpus,removePunctuation)
corpus = tm_map(corpus,stripWhitespace)
corpus = tm_map(corpus,tolower)
corpus = tm_map(corpus,removeWords,stopwords("english"))
wordcloud(corpus)

