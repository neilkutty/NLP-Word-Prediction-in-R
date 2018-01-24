#{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}
#{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}
#{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}
# ____ General / garbage ::::: 
#{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}

quad = readRDS('quadgram.RDS')
a = strsplit(quad$word[1]," ")


a[1]
a[[1]][2:3]

#{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}
#{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}
#{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}



#__________________________________________________________________#
#                ---         Primary Test        ---               #
#------------------------------------------------------------------#
library(ANLP)
source('functions.R')
uni = readRDS('unigram.RDS')
bi = readRDS('bigram.RDS')
tri = readRDS('trigram.RDS')
quad = readRDS('quadgram.RDS')
#penta = readRDS('../pentagram.RDS')
#six = readRDS('../../sixgram.RDS')
#seven = readRDS('../../sevengram.RDS')

ngramlist = c(quad,tri,bi,uni)


testString <- "I can't deal with"
predict_Backoff(testString, ngramlist)

x = "I"
predictions = predict_Backoff(x,ngramlist)

# # # Test function for prediction
predictWord = function(text){
    clean_text = clean_fx(text)
    n = length(clean_text)
    
    if(n==0){
        response = 'Please enter text above...'
    }
    if(n==1){
        response = filter(uni,uni$word==clean_text[1])
    }
    if(n==2){
        
    }
    if(n==3){
        
    }
}

predictWord(x)

#__________________________________________________________________#
#          END   ---   ^ ^ ^ Primary Test ^ ^ ^  ---   END         #
#------------------------------------------------------------------#

# Test
user.input <- "How will the conference go if"
i <- length(strsplit(user.input," ")[[1]])

sub.input <- seq(i-3, i)
user.input <- as.vector(strsplit(user.input," ")[[1]])[sub.input]
i <- length(user.input)

#------------------------------------------
### ------------------------------------------------------------------------------->
#Match string in Four Gram and get probable word
matchinFourGranm <- function (inputWord1,inputWord2,inputWord3)
    
{
    predictWord <- filter(ngramFour,(word1 == inputWord1 & word2 == inputWord2 & word3 == inputWord3))$word4
    if(length(predictWord) == 0)
    {
        
        predictWord <- filter(ngramFour,( word2 == inputWord2 & word3 == inputWord3))$word4
        if(length(predictWord) == 0)
        {
            predictWord <- filter(ngramFour,( word1 == inputWord2 & word2 == inputWord3))$word3
            
            
            if(length(predictWord) ==0)
            {
                predictWord <- matchThreeGram(inputWord2,inputWord3)
            }
            
        }
        
    }
    
    predictWord
    
}
### ------------------------------------------------------------------------------->
# EXAMPLE: sequence of functions from ANLP::cleanTextData


### ------------------------------------------------------------------------------->
#> cleanTextData
function (data) 
{
    data.cleaned <- iconv(data, "latin1", "ASCII", sub = "'")
    corpus <- Corpus(VectorSource(list(data.cleaned)))
    corpus.cl <- tm_map(corpus, removeNumbers)
    corpus.cl <- tm_map(corpus.cl, content_transformer(replace_contraction))
    corpus.cl <- tm_map(corpus.cl, content_transformer(replace_abbreviation))
    corpus.cl <- tm_map(corpus.cl, content_transformer(bracketX))
    corpus.cl <- tm_map(corpus.cl, removePunctuation)
    corpus.cl <- tm_map(corpus.cl, content_transformer(tolower))
    corpus.cl <- tm_map(corpus.cl, stripWhitespace)
    return(corpus.cl)
}

### ------------------------------------------------------------------------------->
#> buildNgramModel
function (N) 
{
    return(function(x) NGramTokenizer(x, Weka_control(min = N, 
                                                      max = N, delimiters = " \\r\\n\\t.,;:\"()?!")))
}


### ------------------------------------------------------------------------------->
#> generateTDM

function (data, N, isTrace = F) 
{
    if (isTrace) {
        startTime = Sys.time()
        print(paste0("Build started: ", N, "-gram model", " @ ", 
                     startTime))
    }
    tdm = TermDocumentMatrix(data, control = list(tokenize = buildNgramModel(N)))
    tdm.df = data.frame(word = tdm$dimnames[[1]], freq = rowSums(as.matrix(tdm)), 
                        row.names = NULL)
    tdm.df = tdm.df[order(-tdm.df$freq), ]
    if (isTrace) {
        print(paste0("Time to build ", N, "-gram model", " :- ", 
                     Sys.time() - startTime))
    }
    return(tdm.df)
}

### ------------------------------------------------------------------------------->
### ------------------------------------------------------------------------------->
### ----------------------ANLP predict_Backoff------------------------------------->
### ------------------------------------------------------------------------------->
### ------------------------------------------------------------------------------->
### ------------------------------------------------------------------------------->
# Get rid of isDebugMode

pBackoff_test = function (testline, modelsList, isDebugMode = F) {
    maxNGramIndex = length(modelsList)
    line = iconv(testline, "latin1", "ASCII", sub = "")
    line = line %>% replace_abbreviation %>% replace_contraction %>% 
        removeNumbers %>% removePunctuation %>% tolower %>% stripWhitespace
   
    #Converts sentence to character vector
    words <- unlist(strsplit(line, split = " "))
    len <- length(words)
    
    if (len < maxNGramIndex) {
        nGramIndex = len + 1
        localModelsList = modelsList[(maxNGramIndex - len):maxNGramIndex]
    }
    else {
        nGramIndex = maxNGramIndex
        localModelsList = modelsList
    }
    for (model in localModelsList) {
        pattern = paste0("^", paste(words[(len - nGramIndex + 
                                               2):len], collapse = " "))
        nextWords = model[grep(pattern, model$word)[1:5], 1]
        nextWords = nextWords[!is.na(nextWords)]
        if (length(nextWords) != 0) {
            ##>Gets one random ngram from the 5 retrieved above<
            nextWordIndex = sample(1:length(nextWords), 1)
            nextWord = nextWords[nextWordIndex]
        }
        else {
            nextWord = NA
        }
        nGramIndex = nGramIndex - 1
        if (!is.na(nextWord)) {
            tempNextWord = unlist(strsplit(as.character(nextWord), 
                                           " "))
            nextWord = paste(tempNextWord[length(tempNextWord)])
            break
        }
    }
    if (is.na(nextWord)) {
         nextWord = modelsList[[maxNGramIndex]][1, 1]
    }
       return(nextWord)
}


#------------------------------------------
#> predict_Backoff

function (testline, modelsList, isDebugMode = F) 
{
    maxNGramIndex = length(modelsList)
    line = iconv(testline, "latin1", "ASCII", sub = "")
    line = line %>% replace_abbreviation %>% replace_contraction %>% 
        removeNumbers %>% removePunctuation %>% tolower %>% stripWhitespace
    if (isDebugMode) 
        print(line)
    words <- unlist(strsplit(line, split = " "))
    len <- length(words)
    if (isDebugMode) 
        print(paste("Length of the string is: ", len))
    if (len < maxNGramIndex) {
        nGramIndex = len + 1
        localModelsList = modelsList[(maxNGramIndex - len):maxNGramIndex]
    }
    else {
        nGramIndex = maxNGramIndex
        localModelsList = modelsList
    }
    if (isDebugMode) 
        print(paste("Number of models will be used: ", length(localModelsList)))
    for (model in localModelsList) {
        pattern = paste0("^", paste(words[(len - nGramIndex + 
                                               2):len], collapse = " "))
        if (isDebugMode) 
            print(pattern)
        nextWords = model[grep(pattern, model$word)[1:5], 1]
        nextWords = nextWords[!is.na(nextWords)]
        if (length(nextWords) != 0) {
            nextWordIndex = sample(1:length(nextWords), 1)
            nextWord = nextWords[nextWordIndex]
        }
        else {
            nextWord = NA
        }
        if (isDebugMode) 
            print(nextWords)
        if (isDebugMode) 
            print(paste("Predicated word: ", nextWord))
        nGramIndex = nGramIndex - 1
        if (!is.na(nextWord)) {
            tempNextWord = unlist(strsplit(as.character(nextWord), 
                                           " "))
            if (isDebugMode) 
                print(paste("Splitted text: ", tempNextWord))
            nextWord = paste(tempNextWord[length(tempNextWord)])
            break
        }
    }
    if (is.na(nextWord)) {
        if (isDebugMode) 
            print(paste("No match found in ", paste(1:maxNGramIndex, 
                                                    collapse = ","), "Gram models so returning the most frequent word"))
        nextWord = modelsList[[maxNGramIndex]][1, 1]
    }
    if (isDebugMode) 
        print(paste("The next predicated word using", nGramIndex + 
                        1, "gram model:-", nextWord))
    return(nextWord)
}
#{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}
#{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}
#{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}

#{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}
#{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}
#{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}

#{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}
#{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}
#{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}{}{}}{}{}{}{}{}{}{}{}
# https://github.com/mhnierhoff/CapstoneCoursera/blob/master/ShinyApp/inputCleaner.R#L45
#---- Another different prediction function than ANLP

nextWordPrediction <- function(wordCount,textInput){
    
    if (wordCount>=3) {
        textInput <- textInput[(wordCount-2):wordCount] 
        
    }
    
    else if(wordCount==2) {
        textInput <- c(NA,textInput)   
    }
    
    else {
        textInput <- c(NA,NA,textInput)
    }
    
    
    ### 1 ###
    wordPrediction <- as.character(final4Data[final4Data$unigram==textInput[1] & 
                                                  final4Data$bigram==textInput[2] & 
                                                  final4Data$trigram==textInput[3],][1,]$quadgram)
    
    if(is.na(wordPrediction)) {
        wordPrediction1 <- as.character(final3Data[final3Data$unigram==textInput[2] & 
                                                       final3Data$bigram==textInput[3],][1,]$trigram)
        
        if(is.na(wordPrediction)) {
            wordPrediction <- as.character(final2Data[final2Data$unigram==textInput[3],][1,]$bigram)
        }
    }
    
    
    print(wordPrediction)
    
}