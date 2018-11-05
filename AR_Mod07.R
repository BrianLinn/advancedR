# Advanced R module 7

# SVM
# Diabetes data - Pima tribe - library MASS
# https://cran.r-project.org/web/packages/MASS/MASS.pdf

library(class)     # KNN
library(e1071)     # SVM
library(MASS)      # data
library(reshape2)  # melt
library(ggplot2)   # graphs


data("Pima.tr")
data("Pima.te")
pima <- rbind(Pima.tr, Pima.te)
# Scale the data - mean 8, sd 1
pimaScale <- as.data.frame(scale(pima[,-8]))
pimaScale$type <- pima$type
View(pimaScale)

# Train and test sets
table(pimaScale$type)
set.seed(123)
ind <- sample(2, nrow(pimaScale), replace = TRUE, prob = c(0.7, 0.3))
train <- pimaScale[ind==1,]
test <- pimaScale[ind==2,]
table(train$type)
table(test$type)

# SVM tuning (e1071)
# Can use kernel polynomial, radial, sigmoid as well
svmTuneLinear <- tune.svm(type~., data = train, kernel = "linear",
                          cost = c(0.001, 0.01, 0.1, 1, 5, 10))
summary(svmTuneLinear)
# best cost 1 in test example

# Create the model
svm1 <- svm(type~., data = train, kernel = "linear", cost = 1)
summary(svm1)

# Predictions
svm1pred <- predict(svm1, newdata = test)
table(svm1pred, test$type)
accuracy <- (85 + 33) / 147
accuracy  # Correct predictions - 0.8027211

# Plot example
plot(svm1, train, glu ~ npreg)


# Time series

# Data frame with time series data
library(RODBC)
con <- odbcConnect("AWDW", uid="RUser", pwd="Pa$$w0rd")
df_TS <- as.data.frame(sqlQuery(con, 
                                "SELECT TimeIndex, SUM(Amount) AS Amount
                                FROM dbo.vTimeSeries
                                GROUP BY TimeIndex
                                ORDER BY TimeIndex"), stringsAsFactors = FALSE)
close (con)
View(df_TS)

# Create a time series 
ts(data = df_TS[,2],
   start=c(2010,12), 
   frequency=12)

# Create a time series object
tsdata <- ts(data = df_TS[,2],
             start=c(2010,12),
             frequency=12)

# Plot the time series
plot(tsdata)

# Couple of ARIMA models
arima(tsdata, order = c(1,0,0))
arima(tsdata, order = c(0,0,1))
arima(tsdata, order = c(1,0,1))
arima(tsdata, order = c(1,1,1))
arima(tsdata, order = c(2,1,1))

# ARIMA object
TS <- arima(tsdata, order = c(1,0,1))

# Forecasts - next 6 months
predict (TS, 6)

# Store forecasts in a list, withut SE
tspredict <- predict (TS, 6, NULL, FALSE)

# Vector of forecasts
c(as.vector(df_TS[,2]), as.vector(tspredict))

# Create a TS object from source data with forecasts added
tsdataforecast <- 
  ts(data = c(as.vector(df_TS[,2]), 
              as.vector(tspredict)),
     start=c(2010,12), frequency=12)

# Plot time series - source data plus forecasts
plot(tsdataforecast)


# Text mining

# Credit
# Copyright © 2013-2014 Graham Williams. You can freely copy, distribute,
# or adapt this material, as long as the attribution is retained and derivative
# work is provided under the same license.
# http://HandsOnDataScience.com/, Graham.Williams@togaware.com, 5th November 2014

# Packages
# install.packages("tm")
# install.packages("SnowballC")
# install.packages("rJava")
# install.packages("qdap")
# install.packages("qdapDictionaries")
# install.packages("dplyr")
# install.packages("RColorBrewer")
# install.packages("ggplot2")
# install.packages("scales")

library(tm)                 # Framework for text mining
library(SnowballC)          # Provides wordStem() for stemming
library(qdap)               # Quantitative discourse analysis of transcripts
library(qdapDictionaries)
library(dplyr)              # Data preparation and pipes %>%
library(RColorBrewer)       # Generate palette of colours for plots
library(ggplot2)            # Plot word frequencies
library(scales)             # Include commas in numbers

# tm sources
getSources()
getReaders()

# Prepare for reading
cname <- file.path("C:\\AdvancedR\\Corpus")
cname
length(dir(cname))
dir(cname)

# Read the documents and check after reading
docs <- Corpus(DirSource(cname))
docs
class(docs)
class(docs[[1]])
summary(docs)
inspect(docs)

# Available transformations
getTransformations()

# Change /,@,| to space
toSpace <- content_transformer(
  function(x, pattern) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
inspect(docs)
docs[[1]]$content

# Lowercase
docs <- tm_map(docs, content_transformer(tolower))
docs[[1]]$content

# Remove numbers
docs <- tm_map(docs, removeNumbers)
docs[[1]]$content

# Remove puctuation
docs <- tm_map(docs, removePunctuation)
docs[[1]]$content

# Remove English stop words
length(stopwords("english"))
stopwords("english")
docs <- tm_map(docs, removeWords, stopwords("english"))
docs[[1]]$content

# Remove own stopwords
docs <- tm_map(docs, removeWords, c("rammer", "sarka"))
docs[[1]]$content

# Strip space
docs <- tm_map(docs, stripWhitespace)
docs[[1]]$content

# Stemming - library(SnowballC)
docs <- tm_map(docs, stemDocument)
inspect(docs)
docs[[1]]$content

# Document term matrix
dtm <- DocumentTermMatrix(docs)
dtm;
inspect(dtm[1:4, 1:5])

# Term frequencies
freq <- colSums(as.matrix(dtm))
length(freq)
# Order by frequency
ord <- order(freq)
# Most frequent terms
freq[tail(ord)]
# Least frequent terms
freq[head(ord)]

# Frequency of term frequencies
head(table(freq), 15)
tail(table(freq), 15)

# Remove sparse terms
dim(dtm)
dtms <- removeSparseTerms(dtm, 0.5)
dim(dtms)
inspect(dtms)

# Can convert document term matrix to a matrix or to a data frame
m <- as.matrix(dtms)
df <- as.data.frame(m)

# Frequencies after removing sparse terms
freq <- colSums(as.matrix(dtms))
freq
table(freq)

# Most frequent terms
findFreqTerms(dtm, lowfreq=20)

# Associations with a given word beyond the correlation limit
findAssocs(dtm, "algorithm", corlimit=0.9)

# Plotting word frequencies
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
head(freq, 14)

wf <- data.frame(word=names(freq), freq=freq)
head(wf)

wf <- subset(wf, freq>18);
ggplot(data=wf, aes(word, freq)) +
  geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

# Word cloud
library(wordcloud)
set.seed(123)
wordcloud(names(freq), freq, min.freq=5)
wordcloud(names(freq), freq, max.words=30)
# Adding colors
wordcloud(names(freq), freq, min.freq=5, 
          colors=brewer.pal(6, "Dark2"))
# Changing scale
wordcloud(names(freq), freq, min.freq=5, 
          scale=c(5, .1), 
          colors=brewer.pal(6, "Dark2"))
# Rotating words
wordcloud(names(freq), freq, min.freq=5, 
          rot.per=0.2, 
          colors=brewer.pal(6, "Dark2"))

# Quantitative analysis - qdap package

# Extract the shorter terms into one long word list
words <- dtm %>%
  as.matrix %>%
  colnames %>%
  (function(x) x[nchar(x) < 20]);
length(words)
summary(nchar(words))
words
table(nchar(words))

# Word length count
df <- data.frame(nletters=nchar(words))
ggplot(data=df, aes(x=nletters)) +
  geom_histogram(binwidth=1) +
  geom_vline(xintercept=mean(nchar(words)),
             colour="green", size=1, alpha=.5) +
  labs(x="Number of Letters", y="Number of Words")

# Letter frequency
library(dplyr)
library(stringr)
# Split words to characters and plot the character frequency
words %>%
  str_split("") %>%
  sapply(function(x) x[-1]) %>%
  unlist %>%
  dist_tab %>%
  mutate(Letter=
           factor(toupper(interval),
                  levels=toupper(interval[order(freq)]))) %>%
  ggplot(aes(Letter, weight=percent)) +
  geom_bar() +
  coord_flip() +
  ylab("Proportion") +
  scale_y_continuous(breaks=seq(0, 12, 2),
                     label=function(x) paste0(x, "%"),
                     expand=c(0,0), limits=c(0,12))

# End of script


