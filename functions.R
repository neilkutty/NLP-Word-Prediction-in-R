# > ----------------------------------------------------------------------------------------------- <
# >             Define functions for creating corpus, tm functions, annotate, extract words
# >     and POS tags. Returns dataframe with words and tags.
# > ----------------------------------------------------------------------------------------------- <
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


### ------------------------------------------------------------------------------->
### ------------------------------------------------------------------------------->
### ------------------------------------------------------------------------------->
 
# 12/22/2017: After adding iconv() below, and rearranging function calls, seems to be
#             getting rid of the emojis, special characters, and whitespace. 
#             However, still returns 57,000 elements for unigram data compared to 12,000
#             returned after running ANLP::cleanTextData() function.  

# Function that performs text specific cleaning. This function is used by mkCorpus below
clean_fx = function(text_char){
    clean_char = text_char %>%
        iconv('latin1', 'ASCII', sub = "'") %>%
        gsub(pattern = "http.*", replacement= "") %>%
        gsub(pattern = "@\\w+", replacement= "") %>%
        gsub(pattern = "by.*", replacement = "") %>%
        gsub(pattern = "\\", replacement = "", fixed = TRUE) %>%
        #gsub(pattern = "[[:digit:]]", replacement = "") %>%
        #gsub(pattern = "[[:punct:]]", replacement = "") %>%
        gsub(pattern = "(RT|via|rt)((?:\\b\\W*@\\w+)+)", replacement = "") %>%
        gsub(pattern = "( rt | rt| rts )", replacement = "")
    return(clean_char)
}

#Function to create corpus from character object, run general text cleaning
# returns: Corpus

# !!! Look at qdap abbreviations functions etc.
mkCorpus <- function(chr){
    #Define corpus
    corp <- NULL
    clean_chr = clean_fx(chr)
    corp <- Corpus(VectorSource(clean_chr))
    corp <- corp %>%
        tm_map(removeNumbers) %>%
        tm_map(content_transformer(replace_contraction)) %>%
        tm_map(content_transformer(replace_abbreviation)) %>%
        tm_map(content_transformer(bracketX)) %>%
        tm_map(tolower) %>%
        tm_map(removePunctuation) %>%
        tm_map(stripWhitespace)
    return(corp)}

#Function to create annotation object 
# returns: annotation object
annotateText <- function(corpus){
    #Create annotators
    words_ann <- Maxent_Word_Token_Annotator()
    sents_ann <- Maxent_Sent_Token_Annotator()
    pos_tag <- Maxent_POS_Tag_Annotator()
    annot <- NLP::annotate(corpus$content, list(sents_ann, words_ann, pos_tag))
    return(annot)
}

#Function that retrieves POS tags and words, returning a Word/Tag dataframe
# returns: dataframe
getWordTags <- function(annotation, corpus){
    #extract words with POS tags
    sWords <- subset(annotation, type=="word")
    sTags <- sapply(sWords$features,'[[','POS' )
    sWordTags <- sprintf("%s[]%s", as.String(corpus$content)[sWords], sTags)
    sWordTags = as.data.frame(sWordTags)
    cleanWordTags = separate(data = sWordTags, col = sWordTags, sep = '\\[]', into = c('Word', 'POS'))
}

# Perform ngram tokenization and return dataframe with counts of phrases
# returns: dataframe
ngramTokenize <- function(corpus,min,max){
    ngram <- NGramTokenizer(corpus$content, Weka_control(min=min,max=max))
    ngdf <- data.frame(word = ngram) %>%
        group_by(word) %>%
        summarise(freq = n()) %>%
        arrange(-freq) %>%
        mutate(word = as.character(word)) %>%
        as.data.frame()# !! Add separation if(tri, then three cols.. quad, then 4 cols, etc.)
    return(ngdf)
}
# > ----------------------------------------------------------------------------------------------- <

