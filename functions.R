

# > ----------------------------------------------------------------------------------------------- <
# >             Define functions for creating corpus, tm functions, annotate, extract words
# >     and POS tags. Returns dataframe with words and tags.
# > ----------------------------------------------------------------------------------------------- <

#Function to create corpus from character object, run general text cleaning
# returns: Corpus
mkCorpus <- function(chr){
    #Define corpus
    corp <- NULL
    corp <- Corpus(VectorSource(chr))
    corp <- tm_map(corp, stripWhitespace) %>%
        tm_map(tolower) %>%
        tm_map(removePunctuation) %>%
        tm_map(removeNumbers)
    return(corp)}


#Function to create annotation object 
# returns: annotation object
annotateText <- function(corpus){
    #Create annotators
    words_ann <- Maxent_Word_Token_Annotator()
    sents_ann <- Maxent_Sent_Token_Annotator()
    pos_tag <- Maxent_POS_Tag_Annotator()
    annot <- annotate(corpus$content, list(sents_ann, words_ann, pos_tag))
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
   ngdf <- data.frame(phrase = ngram) %>%
       group_by(phrase) %>%
       summarise(count = n())
   return(ngdf)
}
# > ----------------------------------------------------------------------------------------------- <