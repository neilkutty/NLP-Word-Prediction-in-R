library(RSQLite)
library(shiny)
library(tm)
library(tm.plugin.mail)
library(RColorBrewer)
library(wordcloud)
library(ggplot2)
library(topicmodels)
library(googleVis)
library(plotly)
options(gsubfn.engine="R")
library(sqldf)
library(openNLP)


sqlite <- dbDriver("SQLite")
db <- dbConnect(sqlite, "../output/database.sqlite")

#########---Extraction- SQL for simple email quantification-########################----------------------###################------------##########

Extraction <- dbGetQuery(db,"SELECT 
                         date(MetadataDateSent) SendDate
                         ,CASE WHEN SenderPersonId = 80 THEN 'From Hillary'
                         ELSE 'To Hillary' END Who 
                         ,CASE WHEN length(RawText) <= 1000 AND length(RawText) > 0 THEN '1 to 1,000'
                         WHEN length(RawText) <= 5000 AND length(RawText) > 1000 THEN '1,001 to 5,000'
                         WHEN length(RawText) <= 10000 AND length(RawText) > 5000 THEN '5,001 to 10,000'
                         WHEN length(RawText) <= 50000 AND length(RawText) > 10000 THEN '10,001 to 50,000'
                         WHEN length(RawText) <= 100000 AND length(RawText) > 50000 THEN '50,001 to 100,000'
                         ELSE '> 100,000' END NumChars  
                         ,length(RawText)-length(replace(RawText,' ','')) + 1 NumberOfWords
                         ,length(RawText) StrLength 
                         ,p.Name Sender
                         ,MetadataTo Receiver
                         ,ExtractedSubject EmailSubject
                         ,ExtractedBodyText EmailBody
                         ,RawText
                         ,substr(RawText,instr(RawText,'To')) Extext
                         FROM Emails e
                         INNER JOIN Persons p ON e.SenderPersonId=p.Id
                         WHERE ExtractedSubject NOT LIKE '%calls%'
                         AND ExtractedSubject NOT LIKE '%schedule%'
                         ORDER BY length(RawText) DESC
                         ")

#--------------------------------------------------------------------------------
##  SQL to run on Extraction dataframe ##
SQL_string <- ("select strftime('%m-%Y', SendDate) as Month
               ,NumChars
               ,NumberOfWords
               ,StrLength
               ,Who
               ,Sender
               ,Receiver
               ,EmailSubject
               ,EmailBody
               from Extraction where SendDate IS NOT NULL
               order by SendDate asc")


subset <- sqldf(SQL_string,stringsAsFactors = FALSE)

subset <- data.frame(subset)

subset$Who <- as.factor(subset$Who)


subset <- sqldf(SQL_string,stringsAsFactors = FALSE)

subset <- data.frame(subset)


#///________________________________________\\\#
# WORKING QPLOT
##$$$$$$$$$$############$$$$$$$$$$#############$$$$$$$$$$$$$$####################
qplot(Extraction$NumChars
      ,Extraction$Who
      ,size=Extraction$NumberOfWords
      ,color=Extraction$StrLength)+theme(axis.text.x = element_text(angle=90,hjust=1))

##
###   GGPLOT   #################
#Working barplot####

profile<-
  
  ggplot(Extraction,aes(x=Extraction$NumChars,y=Extraction$NumberOfWords,group=1)) + 
  geom_dotplot()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

symbols(Extraction$NumChars,Extraction$NumberOfWords,circles=Extraction$NumberOfWords)
#--------------------------------------------------------------------------------
#|||||||||||||||||||||| ----------------------------- ||||||||||||||||||||||||#
##      PLOTLY
#|||||||||||||||||||||| ----------------------------- ||||||||||||||||||||||||#

plot_ly(data = subset, x=Month, y=NumberOfEmails,
        group=NumChars) %>% layout()


#Subplot
##
##
##
##


#()()()______________________________________________________________________()()()#
#()()()  Google Vis ()()()#
#____________________________________________________________________________()()()#
###
##
#

TimeLine <- gvisAnnotationChart(subset,datevar = "SendDate",
                                numvar="NumberOfEmails",idvar="NumChars",date.format = "%Y/%m/%d"
                                ,options=list(
                                  width="800px", height="450px"))

plot(TimeLine)


#Bubble chart

Bubble <- gvisScatterChart(subset)

plot(Bubble)
#____________________________________________________________________________()()()#
#()()()______________________________________________________________________()()()#





#///________________________________________\\\#
###       LDA TOPIC MODELING
#///________________________________________\\\#
k = 4

# Beware: this step takes a lot of patience!  My computer was chugging along for probably 10 or so minutes before it completed the LDA here.
lda.model = LDA(dtm, k)

# This enables you to examine the words that make up each topic that was calculated.  Bear in mind that I've chosen to stem all words possible in this corpus, so some of the words output will look a little weird.
terms(lda.model,20)

# Here I construct a dataframe that scores each document according to how closely its content 
# matches up with each topic.  The closer the score is to 0, the more likely its content matches
# up with a particular topic. 

emails.topics = posterior(lda.model, dtm)$topics
df.emails.topics = as.data.frame(emails.topics)
df.emails.topics = cbind(email=as.character(rownames(df.emails.topics)), 
                         df.emails.topics, stringsAsFactors=F)

##$$$$$$$$$$############$$$$$$$$$$#############$$$$$$$$$$$$$$####################
ggplot(Extraction
       ,aes(x=Extraction$Numchars, stat="bin"
            ,group=1)
       ,color=Extraction$NumChars)+geom_bar()+theme(axis.text.x = element_text(angle=90,hjust=1))
#########--------------------########################----------------------###################------------##########
#Count of emails released in part or in full by date sent
PartOrFull <- dbGetQuery(db,"SELECT 
                         ExtractedReleaseInPartOrFull PartOrFull
                         ,date(MetadataDateSent) DateSent
                         ,count(*) as Qty
                         FROM Emails e
                         GROUP BY date(MetadataDateSent),ExtractedReleaseInPartOrFull
                         ORDER BY count(*) DESC
                         LIMIT 100")


#########--------------------########################----------------------###################------------##########
#########--------------------########################----------------------###################------------##########
#########--------------------########################----------------------###################------------##########
#########--------------------########################----------------------###################------------##########

##       Text Mining --$

#########--------------------########################----------------------###################------------##########


Extraction <- paste(Extraction$EmailBody, collapse=" // ")


###--^^
x_text = Extraction
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

daysofweek <- c("monday","tuesday","wednesday","thursday","friday","saturday","sunday")

monthsofyear <- c("january","february","march","april","may","june","july","auguest",
                  "september","october","november","december","jan","feb","mar","apr",
                  "may","jun","jul","aug","sep","oct","nov","dec")

otherwords <- c("office","autoreply","confirmed","update","followup","talk","draft","pls","thx",
                "statement","called","call","calls","schedule","schedules",
                "meeting","meetings","speech","mini","fyi","notes","today","tomorrow")

#2.) Create control
termcontrol <- list(removePunctuation = TRUE,
                    stopwords = c(daysofweek,monthsofyear,otherwords,stopwords("SMART"),stopwords("english")),
                    removeNumbers = TRUE,
                    wordLengths=c(10,Inf),
                    tolower = TRUE,
                    replace = TRUE)



tdm = TermDocumentMatrix(x_corpus,control = termcontrol)
# define tdm as matrix
m = as.matrix(tdm)
# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE)
# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)


#----$$$$$


########------------^^____________-------------___________^^-----------#########------------^^____________-------------___________^^-----------#
########------------^^____________-------------___________^^-----------#########------------^^____________-------------___________^^-----------#
########------------^^____________-------------___________^^-----------#########------------^^____________-------------___________^^-----------#


#########--------------------########################----------------------###################------------##########



#########--------------------########################----------------------###################------------##########


