##  Load libraries used for this report
library(tm) 
library(stringi)
library(SnowballC)
library(ggplot2)

## Set working directory
setwd("c:\\")
if (!file.exists("tmp")) {
  dir.create("tmp")
}
setwd("c:\\tmp")

## Set URL for download Swiftkey file
fileUrl <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"

## Set file name for the Swiftkey file download
fileName <- "Coursera-SwiftKey.zip"

## Download the Swiftkey file
download.file(fileUrl, destfile=fileName)

## Document when the file has been downloaded
dateDownloaded <- date()
dateDownloaded

## Unzip the Swiftkey file
unzip(fileName, exdir = "c:/tmp")

## Set the folder from where to read all the en_US files 
fileDir <- file.path("c:/tmp", "final", "en_US")
fileDir

## Examine contents of the folder
dir(fileDir)

## Read blogs file
con <- file("c:/tmp/final/en_US/en_US.blogs.txt", open = "rb")
blogs <- readLines(con, encoding="latin1")
close(con)

## Read news file
con <- file("c:/tmp/final/en_US/en_US.news.txt", open = "rb")
news <- readLines(con, encoding="latin1")
close(con)

## Read twitter file
con <- file("c:/tmp/final/en_US/en_US.twitter.txt", open = "rb")
tweets <- readLines(con, encoding="latin1")
close(con)

## Analyzing the data
  
## Investigate the size of Blogs file
length(blogs)
object.size(blogs)

## Investigate the size of News file
length(news)
object.size(news)

## Investigate the size of Twitter file
length(tweets)
object.size(tweets)

## Investigate number of words in Blogs file
## Join the elements of a character vector into one string
blogString <- stri_flatten(blogs, collapse =" ")

## Extracts all words from the string
blogWords <- unlist(stri_extract_words(blogString, locale = "en"))

## Transform strings to lower case to identify unique words correctly
blogWords <- stri_trans_tolower(blogWords, locale = "en")

## Total number of words in blogs 
bwordsNum <- length(blogWords)
bwordsNum

## Unique number of words in blogs  
ubwordsNum <- length(unique(blogWords))
ubwordsNum

## Investigate number of words in News file
## Join the elements of a character vector into one string
newsString <- stri_flatten(news, collapse =" ")

## Extracts all words from the string
newsWords <- unlist(stri_extract_words(newsString, locale = "en"))

## Transform strings to lower case to identify unique words correctly
newsWords <- stri_trans_tolower(newsWords, locale = "en")

## Total number of words in news 
nwordsNum <- length(newsWords)
nwordsNum

## Unique number of words in news  
unwordsNum <- length(unique(newsWords))
unwordsNum

## Investigate number of words in Twitter file
## Join the elements of a character vector into one string
tweetString <- stri_flatten(tweets, collapse =" ")

## Extracts all words from the string
tweetWords <- unlist(stri_extract_words(tweetString, locale = "en"))

## Transform strings to lower case to identify unique words correctly
tweetWords <- stri_trans_tolower(tweetWords, locale = "en")

## Total number of words in tweets
twordsNum <- length(tweetWords)
twordsNum

## Unique number of words in tweets 
utwordsNum <- length(unique(tweetWords))
utwordsNum

## Creating sample of the data
## To make further data processing easier and faster, I've created 
## a random sample of the data by selecting 10000 rows from each file.

## Load all data files and create samples
crps <- Corpus(DirSource(directory="c:/tmp/final/en_US", enco ding = "latin1"), readerControl = list(reader=readPlain, language="en_US"))

## Set seed to provide reproducability of results
set.seed(3523)
crps[[1]] <- sample(crps[[1]], 10000)
crps[[2]] <- sample(crps[[2]], 10000)
crps[[3]] <- sample(crps[[3]], 10000)
length(crps[[1]])
length(crps[[2]])
length(crps[[3]])

## Performing tokenization and profanity filtering

## Remove punctuation from corpus
cleanCrps <- tm_map(crps, removePunctuation)

## Remove numbers from corpus
cleanCrps <- tm_map(cleanCrps, removeNumbers)

## Convert all to lowercase
cleanCrps <- tm_map(cleanCrps, tolower)

## Remove whitespaces from corpus
cleanCrps <- tm_map(cleanCrps, stripWhitespace)

## Remove stopwords from corpus
cleanCrps <- tm_map(cleanCrps, removeWords, stopwords("english"))

## Filter out curse words and profanities

## Set URL for downloading profanity words list
fileUrl <- "http://www.bannedwordlist.com/lists/swearWords.txt"

## Set file name for the curse words file download
fileName <- "c:/tmp/swearWords.txt"

## Download the curse words file
download.file(fileUrl, destfile=fileName)

## Document when the file has been downloaded
dateDownloaded <- date()
dateDownloaded

## Load curse words into variable
curseWords <- read.table("c:/tmp/swearWords.txt")

## Remove profanities from corpus
cleanCrps <- tm_map(cleanCrps, removeWords, curseWords)
cleanCrps <- Corpus(VectorSource(cleanCrps))

## Performing words frequency analysis
Create Term Document Matrix
tdm <- TermDocumentMatrix(cleanCrps)

## Find 20 most frequently used words
wordFreq <- findFreqTerms(tdm, lowfreq=500)
head(wordFreq, 20)

## Create document term matrix for blogs
bdtm <- DocumentTermMatrix(VCorpus(VectorSource(cleanCrps[[1]])))
## Find a frequency of words for blogs dtm and sort it in descending order
bfreq <- sort(colSums(as.matrix(bdtm)), decreasing=TRUE)
head(bfreq)

## Create a data frame from blogs words and their frequencies
bwf <- data.frame(word = names(bfreq), freq = bfreq)
## Create subset of words with a frequency greater than 550
bswf <- subset(bwf, freq > 550)

## Draw a plot for blogs words and their frequencies 
ggplot(bswf, aes(x=word, y=freq), ) + 
geom_bar(stat="identity") + 
theme(axis.text.x=element_text(angle=45, hjust=1))


## Create document term matrix for news
ndtm <- DocumentTermMatrix(VCorpus(VectorSource(cleanCrps[[2]])))
## Find a frequency of words for news dtm and sort it in descending order
nfreq <- sort(colSums(as.matrix(ndtm)), decreasing=TRUE)
head(nfreq)

## Create a data frame from news words and their frequencies
nwf <- data.frame(word = names(nfreq), freq = nfreq)
## Create subset of words with a frequency greater than 550
nswf <- subset(nwf, freq > 300)

## Show a plot of words from news and their frequencies 
ggplot(nswf, aes(x=word, y=freq), ) + 
geom_bar(stat="identity") + 
theme(axis.text.x=element_text(angle=45, hjust=1))

## Create document term matrix for tweets
tdtm <- DocumentTermMatrix(VCorpus(VectorSource(cleanCrps[[3]])))
## Find a frequency of words for tweets dtm and sort it in descending order
tfreq <- sort(colSums(as.matrix(tdtm)), decreasing=TRUE)
head(tfreq)

## Create a data frame from tweet words and their frequencies
twf <- data.frame(word = names(tfreq), freq = tfreq)
## Create subset of words with a frequency greater than 550
tswf <- subset(twf, freq > 200)

## Show a plot of words from tweets and theor frequencies 
ggplot(tswf, aes(x=word, y=freq), ) + 
geom_bar(stat="identity") + 
theme(axis.text.x=element_text(angle=45, hjust=1))

## Future steps and plans for creating a prediction algorithm and Shiny app

## Study materials about NLP, TM package and RWeka package, to learn how to create N-grams
## Create prediction models, test them, evaluate them and optimize them
## Create the Shiny application and its presentation

