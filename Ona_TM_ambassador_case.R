#' Title: HULT Student Ambassadors Characteristics
#' Purpose: A2: Case 1 - Hult International Business School 
#' Author: Maria Andrea V. Ona
#' email: mandreaona@gmail.com
#' Date: February 26, 2021
#'

# Set the working directory 
# The files are unzipped and the working directory is set to where all the text files are located 
setwd("~/Desktop/Text Analytics/hult_NLP_student/cases/session II/student ambassadors")

# Load the libraries
library(ggplot2)
library(ggthemes)
library(qdap)
library(tm)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(stopwords)
library(stringi)
library(pbapply)
library(plotrix)
library(ggalt)


# Import the data
hultdf <- read.csv('final_student_data.csv', header=TRUE)
hultdf$allText <- paste(hultdf$interests, hultdf$bio)

# Check the dimension of the dataset
dim(hultdf) # 85 rows and 13 columns, including combined interests and bio

## EXPLORATORY DATA ANALYSIS 
# Display the proportion by Campus, Program and Region
prop.table(table(hultdf$campus)) # by campus
prop.table(table(hultdf$programTitle)) # by program
prop.table(table(hultdf$namSorCountry.region)) # by region

## SUBSETTING FOR FUTURE USE
# Subset by Campus and InterestsBio
Campus = hultdf[, 3]
InterestBio = hultdf$allText
dfcampus_eda <- data.frame(Campus, InterestBio)

# Subset by Region and InterestsBio
Region = hultdf[, 9]
dfregion_eda <- data.frame(Region, InterestBio)

# Subset by Program and InterestsBio
Program = hultdf[, 2]
dfprogram_eda <- data.frame(Program, InterestBio)



## STOPWORDS
# Check the length of stopwords using the 'SMART' lexicon
length(stopwords(source = 'smart')) #571 words
length(stopwords('en')) #175 words

# Check the gap between two stopwords's lists       
setdiff(
  stopwords('en'), stopwords(source = 'smart')
) 
# [1] "she's"   "he'd"    "she'd"   "he'll"   "she'll"  "shan't"  "mustn't" "when's"  "why's"   "how's"  

# Create custom stop words and add the gap words from above
stops <- c(stopwords(source = 'smart'), "she's", "he'd", "she'd", "he'll", 
           "shan't", "mustn't", "when's", "why's", "how's", 'hult', 'school',
           'london', 'boston', 'dubai', 'san francisco', 'business', 'international',
           'interests', 'degree','time', 'mba','part','ambassador', 'ive', 'day',
           'master','degree','business', 'africa', 'asia','europe','america','north',
           'south','world', 'program')



## OPTIONS AND FUNCTIONS
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')
options(scipen = 999)

tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, content_transformer(replace_contraction)) 
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

# Bigram token maker
bigramTokens <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "), 
         use.names = FALSE)
}


## WORD FREQUENCY MATRIX AND ITS PLOTS
# Create a Pyramid Plot to compare interests and bio
# Bring in our supporting functions
source('~/Desktop/Text Analytics/hult_NLP_student/lessons/Z_otherScripts/ZZZ_supportingFunctions.R')

# Read in Data, clean & organize.  Wrapped in another function for you!
# No qdap? Go to the directly to ZZZ Supporting" & remove  contraction in clean corpus
textA <- cleanMatrix(pth             = 'final_student_data.csv',
                     columnName      = 'interests',
                     collapse        = T, 
                     customStopwords = stops,
                     type = 'TDM', # TDM or DTM
                     wgt = 'weightTf') # weightTfIdf or weightTf

textB <- cleanMatrix(pth        = 'final_student_data.csv',
                     columnName = 'bio',
                     collapse   = T,
                     customStopwords = stops,
                     type = 'TDM', # TDM or DTM
                     wgt = 'weightTf')

df        <- merge(textA, textB, by ='row.names')
names(df) <- c('terms', 'interests', 'bio')

# Calculate the absolute differences among in common terms
df$diff <- abs(df$interests - df$bio)

# Organize df for plotting
df<- df[order(df$diff, decreasing=TRUE), ]
top20 <- df[1:20, ]

# Pyarmid Plot
pyramid.plot(lx         = top20$interests, #left
             rx         = top20$bio,    #right
             labels     = top20$terms,  #terms
             top.labels = c('interests', 'Terms', 'bio'), #corpora
             gap        = 20, # space for terms to be read
             main       = 'Words in Common', # title
             unit       = 'wordFreq')


# Display the most frequent words
# Word count analysis: by Boston Campus and Other Campuses
bosTxt <- subset(dfcampus_eda, dfcampus_eda == 'Boston')
otherTxt <- subset(dfcampus_eda, dfcampus_eda$Campus != 'Boston')

# As of tm version 0.7-3 tabular was deprecated
names(otherTxt)[1] <- 'doc_id'

## Frequency matrix for Other campuses
# Read in Data, clean & organize
txtCorpus <- VCorpus(VectorSource(otherTxt))
txtCorpus <- cleanCorpus(txtCorpus, stops)
WordTDM  <- TermDocumentMatrix(txtCorpus)
WordTDMm <- as.matrix(WordTDM)

# Frequency Data Frame
WordSums <- sort(rowSums(WordTDMm), decreasing = T)
WordFreq <- data.frame(word = names(WordSums), frequency = WordSums)

# Review a section
rownames(WordFreq) <- NULL
head(WordFreq, 20)

# Simple barplot; values greater than 15
topWords      <- subset(WordFreq, WordFreq$frequency >= 15) 
topWords      <- topWords[order(topWords$frequency, decreasing = F),]

# Change to factor for ggplot
topWords$word <- factor(topWords$word, 
                        levels=unique(as.character(topWords$word))) 

# Plot using a ggplot
ggplot(topWords, aes(x = word, y = frequency)) + 
  geom_bar(stat = "identity", fill = 'steelblue') + 
  scale_fill_brewer(palette = "steelblue") +
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label = frequency), colour = "white", hjust = 1.25, size = 3.0)

# Plot the 50 most frequently used words that at least occurred twice
set.seed(1234)
wordcloud(names(WordSums), WordSums, max.words = 50, min.freq = 2,
          colors = brewer.pal(10, "RdBu"), scale = c(2,0.5))


# Display the most frequent words in Boston campus
# As of tm version 0.7-3 tabular was deprecated
names(bosTxt)[1]<-'doc_id' 

# Read in Data, clean & organize
bosCorpus <- VCorpus(VectorSource(bosTxt))
bosCorpus <- cleanCorpus(bosCorpus, stops)
bosTDM  <- TermDocumentMatrix(bosCorpus)
bosTDMm <- as.matrix(bosTDM)

# Frequency Data Frame
bosSums <- sort(rowSums(bosTDMm), decreasing = T)
bosFreq <- data.frame(word = names(bosSums), frequency = bosSums)

# Review a section
rownames(bosFreq) <- NULL
head(bosFreq, 20)

# Simple barplot; values greater than 10
bos_topWords      <- subset(bosFreq, bosFreq$frequency >= 10) 
bos_topWords      <- bos_topWords[order(bos_topWords$frequency, decreasing = F),]


# Change to factor for ggplot
bos_topWords$word <- factor(bos_topWords$word, 
                            levels=unique(as.character(bos_topWords$word))) 

# Plot using a ggplot
ggplot(bos_topWords, aes(x = word, y = frequency)) + 
  geom_bar(stat = "identity", fill = 'steelblue') + 
  scale_fill_brewer(palette = "steelblue") +
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label = frequency), colour = "white", hjust = 1.25, size = 3.0)

# Plot the 50 most frequently used words that at least occurred twice
set.seed(1234)
wordcloud(names(bosSums), bosSums, max.words = 50, min.freq = 2,
          colors = brewer.pal(10, "RdBu"), scale = c(1.5,0.5))

# Make a volatile corpus
bosCorpus <- VCorpus(VectorSource(bosTxt))
otherCorpus <- VCorpus(VectorSource(otherTxt))

# Preprocess the corpus
bosCorpus <- cleanCorpus(bosCorpus, stops)
otherCorpus <- cleanCorpus(otherCorpus, stops)

# Make bi-gram TDM according to the tokenize control & convert it to matrix
bosTDM  <- TermDocumentMatrix(bosCorpus, 
                              control=list(tokenize = bigramTokens))
otherTDM  <- TermDocumentMatrix(otherCorpus, 
                              control=list(tokenize = bigramTokens))
bosTDMm <- as.matrix(bosTDM)
otherTDMm <- as.matrix(otherTDM)

# See a bi-gram for Boston Campus
examplePhrase_b <- grep('international experience', rownames(bosTDMm))
bosTDMm[(examplePhrase_b-2):(examplePhrase_b),1:27]

# See a bi-gram for Other Campuses
examplePhrase_o <- grep('international experience', rownames(otherTDMm))
otherTDMm[(examplePhrase_o-2):(examplePhrase_o),1:56]

# Get Row Sums & organize for Boston Campus
bosTDMv <- sort(rowSums(bosTDMm), decreasing = TRUE)
bosDF   <- data.frame(word = names(bosTDMv), freq = bosTDMv)

# Get Row Sums & organize for Other Campuses
otherTDMv <- sort(rowSums(otherTDMm), decreasing = TRUE)
otherDF   <- data.frame(word = names(otherTDMv), freq = otherTDMv)

# Regular dynamic WC, click the pop-out in the viewer
wordcloud2(data = bosDF[1:27,])
wordcloud2(data = otherDF[1:56,])


# Choose a color & drop light ones
wordcloud2(bosDF[1:50,], 
           color = brewer.pal(8, "Dark2"), 
           backgroundColor = "white",
           shape = 'circle')

wordcloud2(otherDF[1:50,], 
           color = brewer.pal(8, "Dark2"), 
           backgroundColor = "white",
           shape = 'circle')
         
## Comparison Word cloud: Boston vs. Other Campuses
# Read in multiple files as individuals
txtFiles <- list.files(pattern = 'international|experience|student')

for (i in 1:length(txtFiles)){
  assign(txtFiles[i], read.csv(txtFiles[i]))
  cat(paste('read completed:',txtFiles[i],'\n'))
}

# Vector Corpus; omit the meta data
Bos <- VCorpus(VectorSource(bosTxt))
Oth <- VCorpus(VectorSource(otherTxt))

# Clean up the data
Bos <- cleanCorpus(Bos, stops)
Oth <- cleanCorpus(Oth, stops)

# Another way to extract the cleaned text 
Bos <- unlist(pblapply(Bos, content))
Oth <- unlist(pblapply(Oth, content))

# FYI
length(Bos)
length(Oth)

# Instead of 1000 individual documents, collapse each into a single "subject" ie a single document
Bos       <- paste(Bos, collapse = ' ')
Oth       <- paste(Oth, collapse = ' ')

# FYI pt2
length(Bos)
length(Oth)

# Combine the subject documents into a corpus of *2* documents
allCampus<- c(Bos, Oth)
allCampus <- VCorpus((VectorSource(allCampus)))

# Make TDM with a different control parameter
# Tokenization `control=list(tokenize=bigramTokens)`
# You can have more than 1 ie `control=list(tokenize=bigramTokens, weighting = weightTfIdf)`
ctrl      <- list(weighting = weightTfIdf)
CampTDM  <- TermDocumentMatrix(allCampus, control = ctrl)
CampTDMm <- as.matrix(CampTDM)

# Make sure order is the same as the c(objA, objB) on line ~80
colnames(CampTDMm) <- c('Boston', 'Dubai, London and San Francisco')

# Examine
head(CampTDMm)

# Make comparison cloud
comparison.cloud(CampTDMm, 
                 max.words = 80, 
                 random.order = FALSE,
                 title.size = 1.5,
                 colors = brewer.pal(ncol(CampTDMm), "Dark2"), 
                 scale = c(2.5,1))


## Inspect Word Associations for Boston and Other Campuses
# Read in Data, clean & organize
bostxtCorpus_corr <- VCorpus(VectorSource(bosTxt$InterestBio))
bostxtCorpus_corr <- cleanCorpus(bostxtCorpus_corr, stops)
bosWordTDM_corr  <- TermDocumentMatrix(bostxtCorpus_corr)
bosWordTDMm_corr <- as.matrix(bosWordTDM_corr)


associations <- findAssocs(bosWordTDM_corr, 'experience', 0.49)
associations

# Organize the word associations
assocDF <- data.frame(terms = names(associations[[1]]),
                      value = unlist(associations))

assocDF$terms <- factor(assocDF$terms, levels=assocDF$terms)

rownames(assocDF) <- NULL

assocDF

# Make a dot plot
ggplot(assocDF, aes(y = terms)) +
  geom_point(aes(x = value), data = assocDF, col = 'blue') +
  theme_gdocs() + 
  geom_text(aes(x = value, label =value), colour = "blue", hjust = "inward", vjust = "inward" , size = 3)


# Inspect word associations
# Read in Data, clean & organize
othertxtCorpus_corr <- VCorpus(VectorSource(otherTxt$InterestBio))
othertxtCorpus_corr <- cleanCorpus(othertxtCorpus_corr, stops)
otherWordTDM_corr  <- TermDocumentMatrix(othertxtCorpus_corr)
otherWordTDMm_corr <- as.matrix(otherWordTDM_corr)


associations1 <- findAssocs(otherWordTDM_corr, 'experience', 0.35)
associations1

# Organize the word associations
assoc1DF <- data.frame(terms = names(associations1[[1]]),
                      value = unlist(associations1))

assoc1DF$terms <- factor(assoc1DF$terms, levels=assoc1DF$terms)

rownames(assoc1DF) <- NULL

assoc1DF

# Make a dot plot
ggplot(assoc1DF, aes(y = terms)) +
  geom_point(aes(x = value), data = assoc1DF, col = 'orangered3') +
  theme_gdocs() + 
  geom_text(aes(x = value, label =value), colour = "orangered3", hjust = "inward", vjust = "inward" , size = 4)


####################################################################################################################
####################################################################################################################

## FREQUENCY MATRIX FOR REGION AND PROGRAM
# Word count analysis: by Region
euTxt <- subset(dfregion_eda, dfregion_eda$Region == 'Europe')
worldTxt <- subset(dfregion_eda, dfregion_eda$Region != 'Europe')

# Read in Data, clean & organize
worldCorpus <- VCorpus(VectorSource(worldTxt))
worldCorpus <- cleanCorpus(worldCorpus, stops)
world_WordTDM  <- TermDocumentMatrix(worldCorpus)
world_WordTDMm <- as.matrix(world_WordTDM)

# Frequency Data Frame
world_WordSums <- sort(rowSums(world_WordTDMm), decreasing = F)
world_WordFreq <- data.frame(word = names(world_WordSums), frequency = world_WordSums)

# Review a section
rownames(world_WordFreq) <- NULL
head(world_WordFreq, 20)

# Simple barplot; values greater than 20
world_topWords      <- subset(world_WordFreq, world_WordFreq$frequency >= 20) 
world_topWords      <- topWords[order(world_topWords$frequency, decreasing = F),]


# Change to factor for ggplot
world_topWords$word <- factor(world_topWords$word, 
                              levels=unique(as.character(world_topWords$word))) 

ggplot(world_topWords, aes(x = word, y = frequency)) + 
  geom_bar(stat = "identity", fill = 'blue') + 
  scale_fill_brewer(palette = "Green") +
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label = frequency), colour = "white", hjust = 1.25, size = 3.0)

# Plot the 50 most frequently used words that at least occurred twice
set.seed(142)
wordcloud(names(world_WordSums), world_WordSums, max.words = 50, min.freq = 2,
          colors = brewer.pal(10, "RdBu"), scale = c(3.5,0.5))

# Display the most frequent words of Europeans
# As of tm version 0.7-3 tabular was deprecated
names(euTxt)[1]<-'doc_id' 

# Read in Data, clean & organize
euCorpus <- VCorpus(VectorSource(euTxt))
euCorpus <- cleanCorpus(euCorpus, stops)
euTDM  <- TermDocumentMatrix(euCorpus)
euTDMm <- as.matrix(euTDM)

# Frequency Data Frame
euSums <- sort(rowSums(euTDMm), decreasing = T)
euFreq <- data.frame(word = names(euSums), frequency = euSums)

# Review a section
rownames(euFreq) <- NULL
head(euFreq, 20)

# Simple barplot; values greater than 20
eu_topWords      <- subset(euFreq, euFreq$frequency >= 20) 
eu_topWords      <- eu_topWords[order(eu_topWords$frequency, decreasing = F),]


# Change to factor for ggplot
eu_topWords$word <- factor(eu_topWords$word, 
                           levels=unique(as.character(eu_topWords$word))) 

ggplot(eu_topWords, aes(x = word, y = frequency)) + 
  geom_bar(stat = "identity", fill = 'blue') + 
  scale_fill_brewer(palette = "Blues") +
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label = frequency), colour = "white", hjust = 1.25, size = 3.0)

# Plot the 50 most frequently used words that at least occurred twice
set.seed(142)
wordcloud(names(euSums), euSums, max.words = 50, min.freq = 2,
          colors = brewer.pal(10, "RdBu"), scale = c(2,1))

## Comparison Word Cloud: Europe Vs. Other Regions
# Make a volatile corpus
euCorpus <- VCorpus(VectorSource(euTxt))
worldCorpus <- VCorpus(VectorSource(worldTxt))

# Preprocess the corpus
euCorpus <- cleanCorpus(euCorpus, stops)
worldCorpus <- cleanCorpus(worldCorpus, stops)

# Make bi-gram TDM according to the tokenize control & convert it to matrix
euTDM  <- TermDocumentMatrix(euCorpus, 
                             control=list(tokenize = bigramTokens))
worldTDM  <- TermDocumentMatrix(worldCorpus, 
                                control=list(tokenize = bigramTokens))
euTDMm <- as.matrix(euTDM)
worldTDMm <- as.matrix(worldTDM)

# See a bi-gram for boston
examplePhrase_eu <- grep('international experience', rownames(euTDMm))
euTDMm[(examplePhrase_eu-2):(examplePhrase_eu),1:36]
# See a bi-gram for other campuses
examplePhrase_w <- grep('international experience', rownames(worldTDMm))
worldTDMm[(examplePhrase_w-2):(examplePhrase_w),1:47]

# Get Row Sums & organize for boston
euTDMv <- sort(rowSums(euTDMm), decreasing = TRUE)
euDF   <- data.frame(word = names(euTDMv), freq = euTDMv)
# Get Row Sums & organize for other campuses
worldTDMv <- sort(rowSums(worldTDMm), decreasing = TRUE)
worldDF   <- data.frame(word = names(worldTDMv), freq = worldTDMv)

# Regular dynamic WC, click the pop-out in the viewer
wordcloud2(data = euDF[1:36,])
wordcloud2(data = worldDF[1:47,])


# Choose a color & drop light ones
wordcloud2(euDF[1:50,], 
           color = brewer.pal(8, "Dark2"), 
           backgroundColor = "white",
           shape = 'circle')

wordcloud2(worldDF[1:50,], 
           color = brewer.pal(8, "Dark2"), 
           backgroundColor = "white",
           shape = 'circle')

## Word cloud: Boston and other campuses
# Read in multiple files as individuals
txtFiles1 <- list.files(pattern = 'international|experience|student')

for (i in 1:length(txtFiles1)){
  assign(txtFiles1[i], read.csv(txtFiles1[i]))
  cat(paste('read completed:',txtFiles1[i],'\n'))
}

# Vector Corpus; omit the meta data
Europe <- VCorpus(VectorSource(euTxt))
World <- VCorpus(VectorSource(worldTxt))

# Clean up the data
Europe <- cleanCorpus(Europe, stops)
World <- cleanCorpus(World, stops)

# Another way to extract the cleaned text 
Europe <- unlist(pblapply(Europe, content))
World <- unlist(pblapply(World, content))

# FYI
length(Europe)
length(World)

# Instead of 1000 individual documents, collapse each into a single "subject" ie a single document
Europe       <- paste(Europe, collapse = ' ')
World       <- paste(World, collapse = ' ')

# FYI pt2
length(Europe)
length(World)

# Combine the subject documents into a corpus of *2* documents
aroundtheW<- c(Europe, World)
aroundtheW <- VCorpus((VectorSource(aroundtheW)))

# Make TDM with a different control parameter
# Tokenization `control=list(tokenize=bigramTokens)`
# You can have more than 1 ie `control=list(tokenize=bigramTokens, weighting = weightTfIdf)`
ctrl      <- list(weighting = weightTfIdf)
RegionTDM  <- TermDocumentMatrix(aroundtheW, control = ctrl)
RegionTDMm <- as.matrix(RegionTDM)

# Make sure order is the same as the c(objA, objB) on line ~80
colnames(RegionTDMm) <- c('Europe', 'Other regions')

# Examine
head(RegionTDMm)

# Make comparison cloud
comparison.cloud(RegionTDMm, 
                 max.words = 50, 
                 random.order = FALSE,
                 title.size = 1,
                 colors = brewer.pal(ncol(RegionTDMm),"Accent"), 
                 scale = c(3,1))


#### Inspect Word Associations: Europe vs. Other Regions
## Europe:
# Read in Data, clean & organize
eutxtCorpus_corr <- VCorpus(VectorSource(euTxt$InterestBio))
eutxtCorpus_corr <- cleanCorpus(eutxtCorpus_corr, stops)
euWordTDM_corr  <- TermDocumentMatrix(eutxtCorpus_corr)
euWordTDMm_corr <- as.matrix(euWordTDM_corr)


associations_eu <- findAssocs(euWordTDM_corr, 'experience', 0.35)
associations_eu 

# Organize the word associations
eu_assocDF <- data.frame(terms = names(associations_eu[[1]]),
                         value = unlist(associations_eu))

eu_assocDF$terms <- factor(eu_assocDF$terms, levels=eu_assocDF$terms)

rownames(eu_assocDF) <- NULL

eu_assocDF

# Make a dot plot
ggplot(eu_assocDF, aes(y = terms)) +
  geom_point(aes(x = value), data = eu_assocDF, col = '#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x = value, label =value), colour = "red", hjust = "inward", vjust = "inward" , size = 2)


## Other Regions
# Read in Data, clean & organize
othertxtCorpus_corr <- VCorpus(VectorSource(otherTxt$InterestBio))
othertxtCorpus_corr <- cleanCorpus(othertxtCorpus_corr, stops)
otherWordTDM_corr  <- TermDocumentMatrix(othertxtCorpus_corr)
otherWordTDMm_corr <- as.matrix(otherWordTDM_corr)


associations1 <- findAssocs(otherWordTDM_corr, 'experience', 0.35)
associations1

# Organize the word associations
assoc1DF <- data.frame(terms = names(associations1[[1]]),
                       value = unlist(associations1))

assoc1DF$terms <- factor(assoc1DF$terms, levels=assoc1DF$terms)

rownames(assoc1DF) <- NULL

assoc1DF

# Make a dot plot
ggplot(assoc1DF, aes(y = terms)) +
  geom_point(aes(x = value), data = assoc1DF, col = '#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x = value, label =value), colour = "red", hjust = "inward", vjust = "inward" , size = 2)



bbaTxt <- subset(dfprogram_eda, dfprogram_eda$Program == 'Bachelor of Business Administration')
mibTxt <- subset(dfregion_eda, dfprogram_eda$Program == 'Master of International Business')

## Word cloud: Europe vs. Other Regions
# Read in multiple files as individuals
txtFiles2 <- list.files(pattern = 'international|experience|student')

for (i in 1:length(txtFiles2)){
  assign(txtFiles2[i], read.csv(txtFiles2[i]))
  cat(paste('read completed:',txtFiles2[i],'\n'))
}

## Comparison Word Cloud: MIB vs. BBA
# Vector Corpus; omit the meta data
Bba <- VCorpus(VectorSource(bbaTxt))
Mib <- VCorpus(VectorSource(mibTxt))

# Clean up the data
Bba <- cleanCorpus(Bba, stops)
Mib <- cleanCorpus(Mib, stops)

# Another way to extract the cleaned text 
Bba <- unlist(pblapply(Bba, content))
Mib <- unlist(pblapply(Mib, content))

# FYI
length(Bba)
length(Mib)

# Instead of 1000 individual documents, collapse each into a single "subject" ie a single document
Bba       <- paste(Bba, collapse = ' ')
Mib       <- paste(Mib, collapse = ' ')

# FYI pt2
length(Bba)
length(Mib)

# Combine the subject documents into a corpus of *2* documents
allPrograms <- c(Bba, Mib)
allPrograms <- VCorpus((VectorSource(allPrograms)))

# Make TDM with a different control parameter
# Tokenization `control=list(tokenize=bigramTokens)`
# You can have more than 1 ie `control=list(tokenize=bigramTokens, weighting = weightTfIdf)`
ctrl      <- list(weighting = weightTfIdf)
ProgramTDM  <- TermDocumentMatrix(allPrograms, control = ctrl)
ProgramTDMm <- as.matrix(ProgramTDM)

# Make sure order is the same as the c(objA, objB) on line ~80
colnames(ProgramTDMm) <- c('BBA', 'MIB')

# Examine
head(ProgramTDMm)

# Make comparison cloud
comparison.cloud(ProgramTDMm, 
                 max.words = 60, 
                 random.order = FALSE,
                 title.size = 1.5,
                 colors = brewer.pal(ncol(ProgramTDMm),"Set1"), 
                 scale = c(2,1))



mbaTxt <- subset(dfprogram_eda, dfprogram_eda$Program == 'Master of Business Administration')
msbaTxt <- subset(dfregion_eda, dfprogram_eda$Program == 'Master of Business Analytics')

## Word cloud: Europe vs. Other Regions
# Read in multiple files as individuals
txtFiles3 <- list.files(pattern = 'international|experience|student')

for (i in 1:length(txtFiles3)){
  assign(txtFiles3[i], read.csv(txtFiles3[i]))
  cat(paste('read completed:',txtFiles3[i],'\n'))
}

## Comparison Word Cloud: MIB vs. BBA
# Vector Corpus; omit the meta data
mba <- VCorpus(VectorSource(mbaTxt))
msba <- VCorpus(VectorSource(msbaTxt))

# Clean up the data
mba <- cleanCorpus(mba, stops)
msba <- cleanCorpus(msba, stops)

# Another way to extract the cleaned text 
mba <- unlist(pblapply(mba, content))
msba <- unlist(pblapply(msba, content))

# FYI
length(mba)
length(msba)

# Instead of 1000 individual documents, collapse each into a single "subject" ie a single document
mba       <- paste(mba, collapse = ' ')
msba       <- paste(msba, collapse = ' ')

# FYI pt2
length(mba)
length(msba)

# Combine the subject documents into a corpus of *2* documents
allPrograms1 <- c(mba, msba)
allPrograms1 <- VCorpus((VectorSource(allPrograms1)))

# Make TDM with a different control parameter
# Tokenization `control=list(tokenize=bigramTokens)`
# You can have more than 1 ie `control=list(tokenize=bigramTokens, weighting = weightTfIdf)`
ctrl      <- list(weighting = weightTfIdf)
Program1TDM  <- TermDocumentMatrix(allPrograms1, control = ctrl)
Program1TDMm <- as.matrix(Program1TDM)

# Make sure order is the same as the c(objA, objB) on line ~80
colnames(Program1TDMm) <- c('MBA', 'MSBA')

# Examine
head(Program1TDMm)

# Make comparison cloud
comparison.cloud(Program1TDMm, 
                 max.words = 60, 
                 random.order = FALSE,
                 title.size = 1.5,
                 colors = brewer.pal(ncol(Program1TDMm),"Accent"), 
                 scale = c(2,1))


