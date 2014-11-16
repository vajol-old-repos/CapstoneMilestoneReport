## Load libraries
library(tm) 

## Check existence of a working directory
setwd("c:/")
if (!file.exists("tmp")) {
  dir.create("tmp")
}

## Set working directory
setwd("c:/tmp")

## Set URL for download Swiftkey file
fileUrl <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"

## Set file name for the Swiftkey file download
fileName <- "Coursera-SwiftKey.zip"

## Download the Swiftkey file
download.file(fileUrl, destfile=fileName)

## Document when the files have been downloaded
dateDownloaded <- date()
dateDownloaded

## Unzip the Swiftkey file
unzip(fileName, exdir = "c:/tmp")

## Load "tm" library
library(tm) 

## Load "stringi" library
library(stringi)

## Set the folder from where to read all the en_US files 
fileDir <- file.path("c:/tmp", "final", "en_US")
fileDir
## [1] "c:/tmp/final/en_US"

## Examine contents of the folder
dir(fileDir)
## [1] "en_US.blogs.txt"   "en_US.news.txt"    "en_US.twitter.txt"

## Read blogs file
con <- file("c:/tmp/final/en_US/en_US.blogs.txt", open = "rb")
blogs <- readLines(con, encoding="latin1")##"UTF-8")
close(con)

## Read news file
con <- file("c:/tmp/final/en_US/en_US.news.txt", open = "rb")
news <- readLines(con, encoding="latin1")##"UTF-8")
close(con)

## Read twitter file
con <- file("c:/tmp/final/en_US/en_US.twitter.txt", open = "rb")
tweets <- readLines(con, encoding="latin1")##"UTF-8")
close(con)

## Investigate the size of Blogs file
length(blogs)
##[1] 899288
object.size(blogs)
##[1] "260564320"

## Investigate the size of News file
length(news)
##1010242
object.size(news)
##20111392 bytes

## Investigate the size of Twitter file
length(tweets)
##[1] 2360148
object.size(tweets)
##316037344 bytes

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
##[1] 37541795

## Unique number of words in blogs  
ubwordsNum <- length(unique(blogWords))
ubwordsNum
##[1] 318959

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
##[1] 34762303

## Unique number of words in news  
unwordsNum <- length(unique(newsWords))
unwordsNum
##[1] 284463

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
##[1] 30092866

## Unique number of words in tweets 
utwordsNum <- length(unique(tweetWords))
utwordsNum
##[1] 370101

## crps <- Corpus(DirSource(directory="c:/tmp/final/en_US", encoding = "latin1"), readerControl = list(reader=readPlain, language="en_US"))
# Convert all objects to vectors
bvct <- unlist(blogs) 
nvct <- unlist(news) 
tvct <- unlist(tweets) 

## Create sample by selecting randomly 10000 records data from each part
## Set seed to provide reproducability of results
set.seed(3523)
blogSample <- bvct[sample(1:length(bvct), 10000, replace=FALSE)] 
newsSample <- nvct[sample(1:length(nvct), 10000, replace=FALSE)]
tweetSample <- tvct[sample(1:length(tvct), 10000, replace=FALSE)]

blogSample <- sample(blogs, 10000, replace=FALSE)
newsSample <- sample(news,  10000, replace=FALSE)
tweetSample <- sample(tweets,  10000, replace=FALSE)

## blogSample <- blogs[[sample(1:length(blogs), 10000, replace=FALSE)]
## newsSample <- news[[sample(1:length(news), 10000, replace=FALSE)]]
## tweetSample <- tweets[[sample(1:length(tweets), 10000, replace=FALSE)]]


## Combine all three samples together
completeData <- c(blogSample, newsSample, tweetSample)

## Check the size of the complete data
length(completeData)
##[1] 30000

## dataBckp<-file("c:/tmp/sample_data_10k.txt")
## writeLines(completeData, dataBckp)
## close(dataBckp)

## Load the corpus with all sample data
crps <- Corpus(VectorSource(completeData))

# Investigate the structure of a corpus
class(crps)
##[1] "VCorpus" "Corpus"  "list"

length(crps)
##[1] 426968

## Backup sample corpus to the local drive
writeCorpus(crps, path = "c:/tmp/corpus_bckp/corpus1")

## Explore basic text mining transformations and data cleaning
getTransformations()
## [1] "as.PlainTextDocument" "removeNumbers"        "removePunctuation"    "removeWords"          "stemDocument"        
## [6] "stripWhitespace"

## Perform text mining transformations and data cleaning

## Remove punctuation from corpus
cleanCrps <- tm_map(crps, removePunctuation)

## Remove numbers from corpus
cleanCrps <- tm_map(cleanCrps, removeNumbers)

## Convert all to lowercase
## cleanCrps <- tm_map(cleanCrps, tolower)
## cleanCrps <- tm_map(cleanCrps, PlainTextDocument)

## Remove whitespaces from corpus
cleanCrps <- tm_map(cleanCrps, stripWhitespace)

## Remove stopwords from corpus
cleanCrps <- tm_map(cleanCrps, removeWords, stopwords("english"))

## Remove some special words from corpus
cleanCrps[[20480]] <- "is the prettiest girl I know"

length(cleanCrps)

## Filter out curse words and profanities
## Set URL for downloading profanity words list
fileUrl <- "http://www.bannedwordlist.com/lists/swearWords.txt"

## Set file name for the curse words file download
fileName <- "c:/tmp/swearWords.txt"

## Download the curse words file
download.file(fileUrl, destfile=fileName)

## Load curse words into variable
curseWords <- read.table("c:/tmp/swearWords.txt")

## Remove profanities from corpus
cleanCrps <- tm_map(cleanCrps, removeWords, curseWords)

## Backup the corpus
writeCorpus(cleanCrps, path = "c:/tmp/corpus_bckp/corpus2")

## cleanCrps <- Corpus(VectorSource(cleanCrps))
## Creating a Document Term Matrix
dtm <- DocumentTermMatrix(cleanCrps)

tdm <- TermDocumentMatrix(cleanCrps)

dtms <- removeSparseTerms(dtm, 0.1)

#############################################################
install.packages("SnowballC")
library(SnowballC)
crps <- Corpus(DirSource(directory="c:/tmp/final/en_US", enco ding = "latin1"), readerControl = list(reader=readPlain, language="en_US"))
set.seed(3523)
crps[[1]] <- sample(crps[[1]], 10000)
crps[[2]] <- sample(crps[[2]], 10000)
crps[[3]] <- sample(crps[[3]], 10000)
> length(crps[[1]])
[1] 10000
> length(crps[[2]])
[1] 10000
> length(crps[[3]])
[1] 10000
cleanCrps <- Corpus(VectorSource(cleanCrps))
tdm <- TermDocumentMatrix(cleanCrps)
dtm <- DocumentTermMatrix(cleanCrps)
## ?? dtms <- removeSparseTerms(dtm, 0.1)
## ?? tdms <- removeSparseTerms(tdm, 0.1)

> wordFreq <- findFreqTerms(tdm, lowfreq=500)
> head(wordFreq, 20)
[1] "also"  "and"   "back"  "but"   "can"   "day"   "even"  "first" "get"   "going" "good"  "just"  "know"  "last" 
[15] "like"  "made"  "make"  "many"  "may"   "much" 

freq <- sort(colSums(as.matrix(tdm)), decreasing=TRUE)

# Load library RWeka to enable n-gram vector generation
install.packages("RWeka")
library("RWeka") 
#####################################################
## Create document term matrix for blogs
bdtm <- DocumentTermMatrix(VCorpus(VectorSource(cleanCrps[[1]])))
## Find a frequency of words for blogs dtm and sort it in descending order
bfreq <- sort(colSums(as.matrix(bdtm)), decreasing=TRUE)
head(bfreq)
## the  one will just  can like 
## 2090 1399 1213 1140 1124 1087

## Create a data frame from blogs words and their frequencies
bwf <- data.frame(word = names(bfreq), freq = bfreq)
## Create subset of words with a frequency greater than 550
bswf <- subset(bwf, freq > 550)
## Show a plot of words from blogs and theor frequencies 
ggplot(bswf, aes(x=word, y=freq), ) + geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=45, hjust=1))

#####################################################

## Create document term matrix for news
ndtm <- DocumentTermMatrix(VCorpus(VectorSource(cleanCrps[[2]])))
## Find a frequency of words for news dtm and sort it in descending order
nfreq <- sort(colSums(as.matrix(ndtm)), decreasing=TRUE)
head(nfreq)
## the said will  one  new also 
## 2466 2461 1105  837  688  571

## Create a data frame from news words and their frequencies
nwf <- data.frame(word = names(nfreq), freq = nfreq)
## Create subset of words with a frequency greater than 550
nswf <- subset(nwf, freq > 300)
## Show a plot of words from news and theor frequencies 
ggplot(nswf, aes(x=word, y=freq), ) + geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=45, hjust=1))

#####################################################

## Create document term matrix for tweets
tdtm <- DocumentTermMatrix(VCorpus(VectorSource(cleanCrps[[3]])))
## Find a frequency of words for tweets dtm and sort it in descending order
tfreq <- sort(colSums(as.matrix(tdtm)), decreasing=TRUE)
head(tfreq)
## just like love  get  the good 
## 650  537  458  445  434  421

## Create a data frame from tweet words and their frequencies
twf <- data.frame(word = names(tfreq), freq = tfreq)
## Create subset of words with a frequency greater than 550
tswf <- subset(twf, freq > 200)
## Show a plot of words from tweets and theor frequencies 
ggplot(tswf, aes(x=word, y=freq), ) + geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=45, hjust=1))
