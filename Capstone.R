# install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source")
# install.packages("openNLP")
# install.packages("NLP")
# ## additional packages
# install.packages("tm")
# install.packages("stringr")
# install.packages("gsubfn")
# install.packages("plyr")

rm(list=ls())
# --- --- --- 

library(NLP) 
library(openNLP) 
library(openNLPmodels.en) 
library(tm) 
library(stringr) 
library(gsubfn)
library(plyr)
library(dplyr)
library(tidyr)
library(RWeka)
library(magrittr)
library(SnowballC)
source('functions.R')

bcon = file("final/en_US/en_US.blogs.txt")
blog <- readLines(bcon,8000)

ncon = file("final/en_US/en_US.news.txt")
news <- readLines(ncon)

tcon = file("final/en_US/en_US.twitter.txt")
twit <- readLines(tcon)

combined <- rbind(blog, news, twit)


#samp <- as.String(samp)
# -------------------------------------------------------------------- #
# ---- Create Corpus and Exploratory --------------------------------- #
# -------------------------------------------------------------------- #

# fxs <- c(stripWhitespace, tolower, removePunctuation)
# corp <- for(i in 1:length(fxs)){sapply(corp, function(x) tm_map(x,fxs[i]))}
#
#
# ___________________________________________________________________
# ___________________________________________________________________
# --- Run functions on text sample data

bcorp <- mkCorpus(blog)

#Annotate
blog.annotation <- annotateText(bcorp) 

#Get word tags from annotation
blog.word.tags <- getWordTags(blog.annotation, bcorp)

#NGram tokenization
blog.grams <- ngramTokenize(bcorp,3,7)

#Summarize words and POS
blog.word.counts <- blog.word.tags %>%
    group_by(Word, POS) %>%
    summarise(Count = n())

blog.pos.counts <- blog.word.tags %>%
    group_by(POS) %>%
    summarise(count = n())
# ___________________________________________________________________
# Dip Trip
# Flip Fantasia
# ___________________________________________________________________



# -------------------------------------------------------------------- #
# ---- Create Term Document Matrix ----------------------------------- #
# -------------------------------------------------------------------- #

tdm = TermDocumentMatrix(bcorp)

##  --------------- Explore Term Document Matrix --------------------- #
# -------------------------------------------------------------------- #

tdm.v = sort(rowSums(as.matrix(tdm)), decreasing=T)
tdm.df = data.frame(word = names(tdm.v), freq=tdm.v)
head(tdm.df)

# -------------------------------------------------------------------- #
# ---- NGram Tokenization -------------------------------------------- #
# -------------------------------------------------------------------- #
bcorp = Corpus(VectorSource(blog))

#Initial view, all characters of sample  ---- <><><> -- #
allTDM <- TermDocumentMatrix(bcorp)
allTDM.df <- data.frame(word = names(sort(rowSums(as.matrix(allTDM)), decreasing = T)),
                        freq = sort(rowSums(as.matrix(allTDM)), decreasing = T))

#Cleaning
bcorp <- tm_map(bcorp, removePunctuation)
bcorp <- tm_map(bcorp, stemDocument)
bcorp <- tm_map(bcorp, removeNumbers)

ngram <- NGramTokenizer(bcorp$content, Weka_control(min=2, max=2))

## Summary Ngram

ngd <- data.frame(gram = ngram) %>%
    group_by(gram) %>%
    summarise(count = n())

# -------------------------------------------------------------------- #
# ---- Try Different Tokenizers    ----------------------------------- #
# -------------------------------------------------------------------- #



# -------------------------------------------------------------------- #
#  Combine sample text and annotations to create AnnotatedPlainTextDoc
#  Accessor functions will now work: sents(), words(), 
#  as.character(samp_doc) to get just the text
# -------------------------------------------------------------------- #

samp_doc <- AnnotatedPlainTextDocument(samp, annot)

head(sents(samp_doc),2)
head(words(samp_doc),13)
words(samp_doc)[1:4]

samp_doc$annotations
samp_doc$content

# -------------------------------------------------------------------- #
# ---- Explore Annotation Objects 
# -------------------------------------------------------------------- #

blog.words.pos <- sprintf("%s|%s",
                          as.String(blog.corpus$content)[subset(annot, type=="word")],
                          sapply(sWords$features,'[[','POS'))

# Access elements of annotation and show together
sWords <- subset(annot, type=="word")
sTags <- sapply(sWords$features,'[[','POS' )
sWordTags <- sprintf("%s[]%s", as.String(blog.corpus$content)[sWords], sTags)
sWordTags = as.data.frame(sWordTags)
cleanWordTags = separate(data = sWordTags, col = sWordTags,
                         sep = '\\[]', into = c('Word', 'POS'))

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#----------------- Legacy ----------------------------------------# =================|
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

#Manual
blog.corpus <- Corpus(VectorSource(blog))
blog.corpus <- tm_map(blog.corpus, removePunctuation)
blog.corpus <- tm_map(blog.corpus, stemDocument)

#Create annotators for words and sentences
words_ann <- Maxent_Word_Token_Annotator()
sents_ann <- Maxent_Sent_Token_Annotator()
pos_tag <- Maxent_POS_Tag_Annotator()

annot <- annotate(blog.corpus, list(sents_ann, words_ann, pos_tag))

 # Access elements of annotation and show together
sWords <- subset(annot, type=="word")
sTags <- sapply(sWords$features,'[[','POS' )
sWordTags <- sprintf("%s[]%s", as.String(blog.corpus$content)[sWords], sTags)
sWordTags = as.data.frame(sWordTags)
cleanWordTags = separate(data = sWordTags, col = sWordTags,
                         sep = '\\[]', into = c('Word', 'POS'))


# Access sentence elements
sSents <- subset(annot, type=="sentence")
SentTags <- sapply(sSents$features,'[[','POS')
sentences <- as.String(blog.corpus$content)[sSents]
sSentsTags <- sprintf("%s<>%s", as.String(blog.corpus$content)[sSents], SentTags)
sSentsTags <- as.data.frame(sSentsTags)
cleanSentsTags = separate(data = sSentsTags, col = sSentsTags,
                          sep='\\<>', into = c('Sentence', 'POS'))
