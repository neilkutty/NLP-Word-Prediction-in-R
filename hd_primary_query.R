### 
library(RSQLite)
library(tm)
library(tm.plugin.mail)
library(RColorBrewer)
library(wordcloud)
library(SnowballC)
sqlite <- dbDriver("SQLite")
db <- dbConnect(sqlite, "../output/database.sqlite")

#########---Extraction- SQL for simple email quantification-########################----------------------###################------------##########

Extraction <- dbGetQuery(db,
"SELECT 
        date(MetadataDateSent) SendDate
        ,CASE WHEN SenderPersonId = 80 THEN 'From Hillary'
            ELSE 'To Hillary' END Who
        ,CASE WHEN length(RawText) <= 1000 AND length(RawText) > 0 THEN '1 to 1,000'
              WHEN length(RawText) <= 5000 AND length(RawText) > 1000 THEN '1,001 to 5,000'
              WHEN length(RawText) <= 10000 AND length(RawText) > 5000 THEN '5,001 to 10,000'
              WHEN length(RawText) <= 50000 AND length(RawText) > 10000 THEN '10,001 to 50,000'
              WHEN length(RawText) <= 100000 AND length(RawText) > 50000 THEN '50,001 to 100,000'
            ELSE '> 100,000' END NumChars
        ,length(RawText) - length(replace(RawText, ' ', '')) + 1 NumberOfWords
        ,length(ExtractedBodyText) StrLength
        ,p.Name Sender
        ,MetadataTo Receiver
        ,ExtractedSubject EmailSubject
        ,ExtractedBodyText EmailBody
        ,RawText
        ,substr(RawText, instr(RawText, 'To')) Extext
FROM Emails e
INNER JOIN Persons p ON e.SenderPersonId = p.Id
WHERE ExtractedSubject NOT LIKE '%call%'
  AND ExtractedSubject NOT LIKE '%schedule%'
ORDER BY length(RawText) DESC
")




#####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#####
#####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#####
#####%%%%%%%%%%%%%%%%%%%%%%%%  Text Transformation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#####
#####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#####


Extraction$New <- gsub("http*","",Extraction$EmailBody)
Extraction$New <- gsub("www*","",Extraction$New)

###
####
#####
######
####### Text collapse
######
#####
####
###

Extract <- paste(Extraction$New, collapse=" // ")

#CREATE FILE FOR PYTHON REFERENCE
write.table(Extract,file="extract.txt")

#
##
###
####
#####
######
#######
########
#########
##########--^^TEXT TRANSFORMATIONS ___ STILL NEED TO FIGURE OUT REMOVAL OF WEB ADDRESSES, TIME, DATE, ETC. perhaps 
##########--^^                         take a subset of dm where the nchar length in the subset is <X char
x_text = Extract
##x_text = sapply(Extraction, function(x) x$getText())
x_text <- iconv(x_text,to="utf-8-mac")
#x_text= gsub("[[:punct:]]", "",x_text)
#remove email addresses
x_text <- gsub("[[:alnum:]]*@[[:alnum:]]*","",x_text)
#Stemming
x_text <- stemDocument(x_text,language="english")
# create a corpus
x_corpus = Corpus(VectorSource(x_text))
# 
#Define control list for TDM and DTM
#1.) define arrays of non-native stopwords
##########
#########
######## 
#######
######
#####
####
###
##
#

daysofweek <- c("monday","tuesday","wednesday","thursday","friday","saturday","sunday")

monthsofyear <- c("january","february","march","april","may","june","july","auguest",
                  "september","october","november","december","jan","feb","mar","apr",
                  "may","jun","jul","aug","sep","oct","nov","dec")

otherwords <- c("office","autoreply","confirmed","update","followup","talk","draft","pls","thx",
                "statement","called","call","calls","schedule","schedules","president","obama",
                "meeting","meetings","speech","mini","fyi","notes","today","tomorrow","state",
                "department","secretary","clinton","house","people","government","united","states","american",
                "political")



#2.) Create control
termcontrol <- list(removePunctuation = TRUE,
                    stopwords = c(daysofweek,monthsofyear,otherwords,stopwords("SMART"),stopwords("english")),
                    #bounds = list(global = c(5,Inf)),
                    removeNumbers = TRUE,
                    wordLengths=c(5,Inf),
                    tolower = TRUE,
                    replace = TRUE)



tdm = TermDocumentMatrix(x_corpus,control = termcontrol)

# define tdm as matrix
m = as.matrix(tdm)
# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE)
# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)

########------------^^____________-------------___________^^-----------#########------------^^____________-------------___________^^-----------#
########------------^^____________-------------___________^^-----------#########------------^^____________-------------___________^^-----------#
########------------^^____________-------------___________^^-----------#########------------^^____________-------------___________^^-----------#

dm$new <- nchar(as.character(dm$word))


########------------^^____________-------------___________^^-----------#########------------^^____________-------------___________^^-----------#
########------------^^____________-------------___________^^-----------#########------------^^____________-------------___________^^-----------#
########------------^^____________-------------___________^^-----------#########------------^^____________-------------___________^^-----------#




########------------^^____________-------------___________^^-----------#########------------^^____________-------------___________^^-----------#
 ########------------^^____________-------------___________^^-----------#########------------^^____________-------------___________^^-----------#
########------------^^____________-------------___________^^-----------#########------------^^____________-------------___________^^-----------#
