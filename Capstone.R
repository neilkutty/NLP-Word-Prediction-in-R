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
#library(qdap)
#library(gsubfn)

library(plyr)
library(ANLP)
library(NLP) 
library(openNLP) 
library(openNLPmodels.en) 
library(tm) 
library(stringi)
library(stringr) 
library(dplyr)
library(tidyr)
library(RWeka)
library(magrittr)
library(ggplot2)
library(doParallel)
library(grid)
library(gridExtra)
source('functions.R')

options( java.parameters = "-Xmx5g" )
registerDoParallel(makeCluster(detectCores()-1))

bcon = file("final/en_US/en_US.blogs.txt")
blog <- readLines(bcon, encoding = "UTF-8", skipNul = TRUE)
close.connection(bcon)

ncon = file("final/en_US/en_US.news.txt")
news <- readLines(ncon, encoding = "UTF-8", skipNul = TRUE)
close.connection(ncon)

tcon = file("final/en_US/en_US.twitter.txt")
twit <- readLines(tcon, encoding = "UTF-8", skipNul = TRUE)
close.connection(tcon)
#Get Sample Stats

#Combine character vectors as one
text_df = c(blog, news, twit)

# bsamp = sample(blog, 50000)
# nsamp = sample(news, 50000)
# tsamp = sample(twit, 50000)

# Get sample from the full text_df
fullsamp = sample(text_df, 100000)

# Create corpus out of the text data sample
fullcorp = mkCorpus(fullsamp)

#Clean up unneeded objects after corpus creation
rm(list=c('blog','twit','news','text_df'))

#x = as.frame(fullcorp$content)

#Initial view, all characters of sample  ---- <><><> -- #
# allTDM <- TermDocumentMatrix(fullcorp)
# allTDM.df <- data.frame(word = names(sort(rowSums(as.matrix(allTDM)), decreasing = T)),
#                         freq = sort(rowSums(as.matrix(allTDM)), decreasing = T))

# *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *
# *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# # Using ANLP version of cleanTextData()
# 
# x = cleanTextData(fullsamp)
# xc = Corpus(VectorSource(x))
# 
# anlp_unigram = ngramTokenize(xc,1,1)
# ## Why does ANLP return 7787 and mine returns 65565 ??


# # # # Need to get rid of backslashes within cleaning functions . . . 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *
# *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *
unigram = ngramTokenize(fullcorp,1,1)
bigram = ngramTokenize(fullcorp,2,2)
trigram = ngramTokenize(fullcorp,3,3)
quadgram = ngramTokenize(fullcorp,4,4)
pentagram = ngramTokenize(fullcorp,5,5)
sixgram = ngramTokenize(fullcorp,6,6)
sevengram = ngramTokenize(fullcorp,7,7)


# bigram_sep = bigram %>%
#     separate(word, into = c('word1','word2'))
# 
# 
# trigram_sep = trigram %>%
#     separate(word, into = c('word1','word2','word3'))
# 
# 
# quadgram_sep = quadgram %>%
#     separate(word, into = c('word1','word2','word3','word4'))
# 
# 
# pentagram_sep = pentagram %>%
#     separate(word, into = c('word1','word2','word3','word4','word5'))


# N-gram data without separation of words in string
saveRDS(unigram, file="unigram.RDS")
saveRDS(bigram_sep, file="bigram.RDS")
saveRDS(trigram_sep, file="trigram.RDS")
saveRDS(quadgram_sep, file="quadgram.RDS")
saveRDS(pentagram_sep, file="pentagram.RDS")
# saveRDS(sixgram, file="sixgram.RDS")
# saveRDS(sevengram, file="sevengram.RDS")

unigram = readRDS(file="unigram.RDS")
bigram = readRDS(file="bigram.RDS")
trigram = readRDS(file="trigram.RDS")
quadgram = readRDS(file="quadgram.RDS")
pentagram = readRDS(file="pentagram.RDS")

# -- Load data and proceed -- <>

ngramlist = list(pentagram, quadgram, trigram, bigram, unigram)

# # ngramlist = list(sevengram,sixgram,pentagram,quadgram,
#                  trigram,bigram,unigram)

testString <- "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each "
predict_Backoff(testString, ngramlist)

x = "I have no idea how"
predict_Backoff(x,ngramlist)
# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>><<<>>><<<>>>
# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>><<<>>>
# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>><<<>>><<<>>>
# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>><<<>>>
# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>><<<>>><<<>>>

#match word in ngram df
j = quadgram[grep('how are you', quadgram$word)[1:5],1]
k = quadgram[grep(inputNew,quadgram$word)[1:5],1]
#jj = j[!is.na(j)]

splitj = unlist(strsplit(as.character(j[1]), " "))
nextj = splitj[length(splitj)]
#--------------------------------------------------------------------------------------------




# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>><<<>>><<<>>>
# successful test of method to get tail of long input (> 3 words/grams)
longTest = 'why is it that how are you'
inputlen = length(unlist(strsplit(as.character(longTest), " ")))

testTail = unlist(strsplit(as.character(longTest),' '))
testTain = testTail[(length(testTail)-2):length(testTail)]
testTain = paste(testTain, collapse = ' ')
#Trying to match tail to parts of prediction
longPred = quadgram[grep(testTain,quadgram$word)[1:5],1]()
longPred = unlist(strsplit(as.character(longPred[1]), " "))
longPred[length(longPred)]
#       Testing prediction function

testString = 'how are you'

predictText = function(testString){
    # j = NULL
    # inputNew = NULL
    # nextWord = NULL
    
    inputlen = length(unlist(strsplit(as.character(testString), " ")))
    
    if (inputlen > 3) {
        inputNew = unlist(strsplit(as.character(testString), " "))
        inputNew = inputNew[(length(inputNew)-2):length(inputNew)]
        inputNew = paste(inputNew, collapse = " ")
        j = quadgram[grep(inputNew,quadgram$word)[1:5],1]
        j = unlist(strsplit(as.character(j[1])," "))
        nextWord = j[length(j)]
    }
    else if (inputlen == 3) {
        inputNew = unlist(strsplit(as.character(testString), " "))
        inputNew = paste(inputNew, collapse = " ")
        j = quadgram[grep(inputNew,quadgram$word)[1:5],1]
        j = unlist(strsplit(as.character(j[1])," "))
        nextWord = j[length(j)]
    }
    else if (inputlen == 2) {
        inputNew = unlist(strsplit(as.character(testString), " "))
        inputNew = paste(inputNew, collapse = " ")
        j = trigram[grep(inputNew,trigram$word)[1:5],1]
        j = unlist(strsplit(as.character(j[1])," "))
        nextWord = j[length(j)]
    }
    else if (inputlen == 1) {
        inputNew = unlist(strsplit(as.character(testString), " "))
        inputNew = paste(inputNew, collapse = " ")
        j = bigram[grep(inputNew,bigram$word)[1:5],1]
        j = unlist(strsplit(as.character(j[1])," "))
        nextWord = j[length(j)]
    }
    else {
        nextWord = 'Please enter text to predict next word...'
    }
return(nextWord)
}

predictText(longTest)
predictText('all of the why is it')
# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>><<<>>><<<>>>
# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>><<<>>>
# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>><<<>>><<<>>>
# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>><<<>>>
# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>><<<>>><<<>>>

# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>><<<>>><<<>>>
# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>><<<>>>
# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>><<<>>><<<>>>
# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>><<<>>>
# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>><<<>>><<<>>>
# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>><<<>>><<<>>>
# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>><<<>>>
# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>><<<>>><<<>>>
# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>><<<>>>
# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>><<<>>><<<>>>

# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>><<<>>><<<>>>
# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>><<<>>>
# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>><<<>>><<<>>>
# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>><<<>>>
# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>><<<>>><<<>>>

# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>><<<>>><<<>>>
# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>><<<>>>
# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>><<<>>><<<>>>
# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>><<<>>>
# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>><<<>>><<<>>>
# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>><<<>>><<<>>>
# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>><<<>>>
# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>><<<>>><<<>>>
# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>><<<>>>
# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>><<<>>><<<>>>
#    -  Clean Text data  -

# 11/10  currently trouble getting rid of retweet text (think resolved)
# 11/12 last gsub pattern seems to be successful with retweets
#       should i just take out the rt part leave text?
# 

# Can whitespace be added when needed ie "thisisthat"
#  is @\\w really getting rid of 

#
# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>>
# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>>
# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>>
# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>>

#functionalize text cleaning
clean_fx = function(text_char){
    clean_char = text_char %>%
        gsub(pattern = "http.*", replacement= "") %>%
        gsub(pattern = "@\\w+", replacement= "") %>%
        gsub(pattern = "by.*", replacement = "") %>%
        gsub(pattern = "[[:digit:]]", replacement = "") %>%
        gsub(pattern = "[[:punct:]]", replacement = "") %>%
        gsub(pattern = "(RT|via|rt)((?:\\b\\W*@\\w+)+)", replacement = "") %>%
        gsub(pattern = "( rt | rt| rts )", replacement = "")
    return(clean_char)
}

text_clean = clean_fx(text_df)


#Check clean text


text_clean[grep(' rts', text_clean)]

text_clean = content_transformer(clean_fx)


tsamp_clean = tsamp %>%
    gsub(pattern = "http.*", replacement= "") %>%
    gsub(pattern = "@\\w+", replacement= "") %>%
    gsub(pattern = "by.*", replacement = "") %>%
    gsub(pattern = "[[:digit:]]", replacement = "") %>%
    gsub(pattern = "[[:punct:]]", replacement = "") %>%
    gsub(pattern = "(RT|via|rt)((?:\\b\\W*@\\w+)+)", replacement = "")

tscorp <- mkCorpus(tsamp_clean)
tcorp_content <- tscorp$content
tcorp_content[grep(' rt ',tcorp_content)]

    
#Remove links and retweets    
# tsamp = gsub("http.*", "",tsamp)
# tsamp = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tsamp)
#  'http([^ ]+)|(RT|via)((?:\\b\\W*@\\w+)+)|@\\w+|[[:digit:]]|[[:punct:]]'
# # remove @people
# tsamp = gsub("@\\w+", "", tsamp)
# 
# # remove numbers
# tsamp = gsub("[[:digit:]]", "", tsamp)
# 
# # remove hashtags NNK 6-2-2015
# tsamp = gsub("#.*","", tsamp)
# 
# #remove author names that start with "By" 
# tsamp = gsub("by.*","", tsamp)
# 
# # remove punctuation symbols
# tsamp = gsub("[[:punct:]]", "", tsamp)    





# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>>
# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>>
# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>>

# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>>
# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>>
# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>>

# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>>
# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>>
# <><><><><><><>>><>><><><>>><><><><><><><>>><><><><><><>>>><<<<>>><><><><><<<>>>



    
    
# -------------------------------------------------------------------- #
#           samp <- as.String(samp)
# -------------------------------------------------------------------- #
# ----   Create Corpus and Exploratory       ------------------------- #
# -------------------------------------------------------------------- #


# fxs <- c(stripWhitespace, tolower, removePunctuation)
# corp <- for(i in 1:length(fxs)){sapply(corp, function(x) tm_map(x,fxs[i]))}
#
#
# ___________________________________________________________________
# ___________________________________________________________________
# --- Run functions on text sample data

bscorp <- mkCorpus(bsamp)
nscorp <- mkCorpus(nsamp)
tscorp <- mkCorpus(tsamp)

#Annotate (not working currently 8/7)
blog.annotation <- annotateText(bscorp) 
news.annotation <- annotateText(nscorp)
twit.annotation <- annotateText(tscorp)

#NGram tokenization
blog.grams <- ngramTokenize(bscorp,3,4)
blog.grams <- blog.grams[order(-blog.grams$count),]

news.grams <- ngramTokenize(nscorp,3,4)
news.grams <- news.grams[order(-news.grams$count),]

twit.grams <- ngramTokenize(tscorp,3,4)
twit.grams <- twit.grams[order(-twit.grams$count),]

#Get word tags from annotation
blog.word.tags <- getWordTags(blog.annotation, bscorp)
news.word.tags <- getWordTags(news.annotation, nscorp)
twit.word.tags <- getWordTags(twit.annotation, tscorp)

#Summarize words and POS tag counts
blog.word.counts <- blog.word.tags %>%
    group_by(Word) %>%
    summarise(Count = n()) 

blog.word.counts <- blog.word.counts[order(-blog.word.counts$Count),]

blog.pos.counts <- blog.word.tags %>%
    group_by(POS) %>%
    summarise(count = n())

blog.pos.counts <- blog.pos.counts[order(-blog.pos.counts$count),]

news.word.counts <- news.word.tags %>%
    group_by(Word) %>%
    summarise(Count = n())

news.word.counts <- news.word.counts[order(-news.word.counts$Count),]

news.pos.counts <- news.word.tags %>%
    group_by(POS) %>%
    summarise(count = n())

news.pos.counts <- news.pos.counts[order(-news.pos.counts$count),]

twit.word.counts <- twit.word.tags %>%
    group_by(Word) %>%
    summarise(Count = n())

twit.word.counts <- twit.word.counts[order(-twit.word.counts$Count),]

twit.pos.counts <- twit.word.tags %>%
    group_by(POS) %>%
    summarise(count = n())

twit.pos.counts <- twit.pos.counts[order(-twit.pos.counts$count),]
# _______________________________________________________________________________
# Dip Trip          --- Primary Exploratory Data ---
#
##                          Exploratory Plots
# _______________________________________________________________________________
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- <<<>
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- <<<>
# --- Word Counts --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- <<<>
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- <<<>
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- <<<>

p1 <- ggplot(data=head(blog.word.counts,n=10), aes(x=reorder(Word,-Count),y=Count))+
    geom_bar(stat = "identity", fill='red')+
    ggtitle(label = 'Top 10 Most Frequent Words in Blog data')+
    geom_text(label=head(blog.word.counts,n=10)$Count,nudge_y = 0.2)+
    xlab(label = 'Word')+
    ylab(label = "Frequency")

p2 <- ggplot(data=head(news.word.counts,n=10), aes(x=reorder(Word,-Count),y=Count))+
    geom_bar(stat = "identity", fill='green')+
    ggtitle(label = 'Top 10 Most Frequent Parts-Of-Speech in News data')+
    geom_text(label=head(news.word.counts,n=10)$Count,nudge_y = 0.2)+
    xlab(label = 'Word')+
    ylab(label = "Frequency")

p3 <- ggplot(data=head(twit.word.counts,n=10), aes(x=reorder(Word,-Count),y=Count))+
    geom_bar(stat = "identity", fill='blue')+
    ggtitle(label = 'Top 10 Most Frequent Parts-Of-Speech in Twitter data')+
    geom_text(label=head(twit.word.counts,n=10)$Count,nudge_y = 0.2)+
    xlab(label = 'Word')+
    ylab(label = "Frequency")

g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g3 <- ggplotGrob(p3)

grid.draw(rbind(g1,g2,g3, size='last'))

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- <<<>
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- <<<>
# --- POS Counts --- --- --- --- --- --- --- --- --- --- --- --- --- -- --- <<<>
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- <<<>
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- <<<>

p4 <- ggplot(data=head(blog.pos.counts,n=10), aes(x=reorder(POS,-count),y=count))+
    geom_bar(stat = "identity", fill='red')+
    ggtitle(label = 'Top 10 Most Frequent Parts-Of-Speech in Blog data')+
    geom_text(label=head(blog.pos.counts,n=10)$count,nudge_y = 0.2)+
    xlab(label = 'POS')+
    ylab(label = "Frequency")

p5 <- ggplot(data=head(news.pos.counts,n=10), aes(x=reorder(POS,-count),y=count))+
    geom_bar(stat = "identity", fill='green')+
    ggtitle(label = 'Top 10 Most Frequent Parts-Of-Speech in News data')+
    geom_text(label=head(news.pos.counts,n=10)$count,nudge_y = 0.2)+
    xlab(label = 'POS')+
    ylab(label = "Frequency")

p6 <- ggplot(data=head(twit.pos.counts,n=10), aes(x=reorder(POS,-count),y=count))+
    geom_bar(stat = "identity", fill='blue')+
    ggtitle(label = 'Top 10 Most Frequent Parts-Of-Speech in Twitter data')+
    geom_text(label=head(twit.pos.counts,n=10)$count,nudge_y = 0.2)+
    xlab(label = 'POS')+
    ylab(label = "Frequency")

g4 <- ggplotGrob(p4)
g5 <- ggplotGrob(p5)
g6 <- ggplotGrob(p6)

grid.draw(rbind(g4,g5,g6, size='last'))

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- <<<>
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- <<<>
# --- Grams --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- <<<>
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- <<<>
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- <<<>

p7 <- ggplot(data=head(blog.grams,n=10), aes(x=reorder(phrase,-count),y=count))+
    geom_bar(stat = "identity", fill='red')+
    ggtitle(label = 'Top 10 Most Frequent 3-4 Size Ngrams in Blog data')+
    xlab(label = 'Phrase')+
    ylab(label = "Frequency")

p8 <- ggplot(data=head(news.grams,n=10), aes(x=reorder(phrase,-count),y=count))+
    geom_bar(stat = "identity", fill='green')+
    ggtitle(label = 'Top 10 Most Frequent 3-4 Size Ngrams in News data')+
    xlab(label = 'Phrase')+
    ylab(label = "Frequency")

p9 <- ggplot(data=head(twit.grams,n=10), aes(x=reorder(phrase,-count),y=count))+
    geom_bar(stat = "identity", fill='blue')+
    ggtitle(label = 'Top 10 Most Frequent 3-4 Size Ngrams in Twitter data')+
    xlab(label = 'Phrase')+
    ylab(label = "Frequency")

g7 <- ggplotGrob(p7)
g8 <- ggplotGrob(p8)
g9 <- ggplotGrob(p9)

grid.draw(rbind(g7,g8,g9, size='last'))

