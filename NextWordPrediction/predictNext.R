#  Prediction function for Shiny app
#   author: Neil Kutty
#   
#  
#



# load(file = 'unigram.Rdata')
# load(file = 'bigram.Rdata')
# load(file = 'trigram.Rdata')
# load(file = 'quadgram.Rdata')
# load(file = 'pentagram.Rdata')
unigram = readRDS(file="unigram.RDS")
bigram = readRDS(file="bigram.RDS")
trigram = readRDS(file="trigram.RDS")
quadgram = readRDS(file="quadgram.RDS")
pentagram = readRDS(file="pentagram.RDS")

predictNext = function(testString, testLength){
    j = NULL
    inputNew = NULL
    nextWord = NULL
    
    inputlen = testLength
    inputlen = length(unlist(strsplit(as.character(testString), " ")))
    
    if (inputlen > 4) {
        inputNew = unlist(strsplit(as.character(testString), " "))
        inputNew = inputNew[(length(inputNew)-3):length(inputNew)]
        inputNew = paste(inputNew, collapse = " ")
        j = pentagram[grep(paste0('^(',inputNew,')'),pentagram$word),1][1]
        j = unlist(strsplit(as.character(j)," "))
        nextWord = j[length(j)]
        if (is.na(nextWord)){
            inputNew = unlist(strsplit(as.character(testString), " "))
            inputNew = inputNew[(length(inputNew)-2):length(inputNew)]
            inputNew = paste(inputNew, collapse = " ")
            j = quadgram[grep(paste0('^(',inputNew,')'), quadgram$word),1][1]
            j = unlist(strsplit(as.character(j)," "))
            nextWord = j[length(j)]
            if (is.na(nextWord)){
                inputNew = unlist(strsplit(as.character(testString), " "))
                inputNew = inputNew[(length(inputNew)-1):length(inputNew)]
                inputNew = paste(inputNew, collapse = " ")
                j = trigram[grep(paste0('^(',inputNew,')'), trigram$word), 1][1]
                j = unlist(strsplit(as.character(j)," "))
                nextWord = j[length(j)]
                if (is.na(nextWord)){
                    inputNew = unlist(strsplit(as.character(testString), " "))
                    inputNew = inputNew[length(inputNew)]
                    inputNew = paste(inputNew, collapse = " ")
                    j = bigram[grep(paste0('^(',inputNew,')'), bigram$word),1][1]
                    j = unlist(strsplit(as.character(j)," "))
                    nextWord = j[length(j)]
                    if(is.na(nextWord)){
                        nextWord = unigram$word[sample(6,1)]
                        
                    }
                    
                }
                
            }
        }
    }
    
    else if (inputlen == 4) {
        inputNew = testString
        j = pentagram[grep(paste0('^(',inputNew,')'),pentagram$word),1][1]
        j = unlist(strsplit(as.character(j)," "))
        nextWord = j[length(j)]
        if (is.na(nextWord)){
            inputNew = unlist(strsplit(as.character(testString), " "))
            inputNew = inputNew[(length(inputNew)-1):length(inputNew)]
            inputNew = paste(inputNew, collapse = " ")
            j = quadgram[grep(paste0('^(',inputNew,')'), quadgram$word),1][1]
            j = unlist(strsplit(as.character(j)," "))
            nextWord = j[length(j)]
            if (is.na(nextWord)){
                inputNew = unlist(strsplit(as.character(testString), " "))
                inputNew = inputNew[(length(inputNew)-2):length(inputNew)]
                inputNew = paste(inputNew, collapse = " ")
                j = trigram[grep(paste0('^(',inputNew,')'), trigram$word),1][1]
                j = unlist(strsplit(as.character(j)," "))
                nextWord = j[length(j)]
                if (is.na(nextWord)){
                    inputNew = unlist(strsplit(as.character(testString), " "))
                    inputNew = inputNew[length(inputNew)]
                    inputNew = paste(inputNew, collapse = " ")
                    j = bigram[grep(paste0('^(',inputNew,')'), bigram$word),1][1]
                    j = unlist(strsplit(as.character(j)," "))
                    nextWord = j[length(j)]
                    if(is.na(nextWord)){
                        nextWord = unigram$word[sample(6,1)]

                }

            }
        }

        }
    }
    else if (inputlen == 3) {
        inputNew = testString
        j = quadgram[grep(paste0('^(',inputNew,')'),quadgram$word),1][1]
        j = unlist(strsplit(as.character(j)," "))
        nextWord = j[length(j)]
        if (is.na(nextWord)){
            inputNew = unlist(strsplit(as.character(testString), " "))
            inputNew = inputNew[(length(inputNew)-1):length(inputNew)]
            inputNew = paste(inputNew, collapse = " ")
            j = trigram[grep(paste0('^(',inputNew,')'), trigram$word), 1][1]
            j = unlist(strsplit(as.character(j)," "))
            nextWord = j[length(j)]
            if (is.na(nextWord)){
                inputNew = unlist(strsplit(as.character(testString), " "))
                inputNew = inputNew[length(inputNew)]
                inputNew = paste(inputNew, collapse = " ")
                j = bigram[grep(paste0('^(',inputNew,')'), bigram$word), 1][1]
                j = unlist(strsplit(as.character(j)," "))
                nextWord = j[length(j)]
                if(is.na(nextWord)){
                    nextWord = unigram$word[sample(6,1)]
                }
            }
        }
    }
    else if (inputlen == 2) {
        inputNew = testString
        j = trigram[grep(paste0('^(',inputNew,')'),trigram$word),1][1]
        j = unlist(strsplit(as.character(j)," "))
        nextWord = j[length(j)]
        if (is.na(nextWord)){
            inputNew = unlist(strsplit(as.character(testString), " "))
            inputNew = inputNew[length(inputNew)]
            inputNew = paste(inputNew, collapse = " ")
            j = bigram[grep(paste0('^(',inputNew,')'), bigram$word), 1][1]
            j = unlist(strsplit(as.character(j)," "))
            nextWord = j[length(j)]
            if(is.na(nextWord)){
                nextWord = unigram$word[sample(6,1)]
            }
        }
    }
    
    else if (inputlen == 1) {
        inputNew = testString
        j = bigram[grep(paste0('^(',inputNew,')'),bigram$word),1][1]
        j = unlist(strsplit(as.character(j)," "))
        nextWord = j[length(j)]
        if(is.na(nextWord)){
            nextWord = unigram$word[sample(6,1)]
        }
    }
    else {
        nextWord = 'App is ready! Please enter text in the box, and the next predicted word will appear here...'
    }
    return(nextWord)
}
