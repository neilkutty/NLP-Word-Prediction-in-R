
#----------------------------------------------------------------------------------------#

rm(list=ls())
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

bcon = file("final/en_US/en_US.blogs.txt")
ncon = file("final/en_US/en_US.news.txt")
tcon = file("final/en_US/en_US.twitter.txt")


blog <- readLines(bcon, 1000)
news <- readLines(ncon, 1000)
twit <- readLines(tcon, 100)

#samp <- as.String(samp)
corp <- NULL


#----------------------------------------------------------------------------------------#


f <- function(queryHistoryTab, query, n = 2) {
    require(tau)
    trigrams <- sort(textcnt(rep(tolower(names(queryHistoryTab)),queryHistoryTab),
                             method = "string", n = length(scan(text = query,
                                                                what = "character",
                                                                quiet = TRUE)) + 1))
    query <- tolower(query)
    idx <- which(substr(names(trigrams), 0, nchar(query)) == query)
    res <- head(names(sort(trigrams[idx], decreasing = TRUE)), n)
    res <- substr(res, nchar(query) + 2, nchar(res))
    return(res)
}
f(c("Can of butt" = 3, "can of butt" = 2, "A can of water butt" = 1, "Buy me a can of soda, please" = 2), "Buy me")
# [1] "soda" "beer"



