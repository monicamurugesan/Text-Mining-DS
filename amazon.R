install.packages(c("rvest","XML","magrittr","xml2"))

library(rvest)
library(XML)
library(xml2)
library(magrittr)

# Amazon Reviews #############################

aurl <- "https://amazon.in/Apple-MacBook-Air-13-3-inch-Integrated/product-reviews/B073Q5R6VR/ref=cm_cr_arp_d_paging_btm_3?showViewpoints=1&pageNumber"
amazon_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>% html_nodes(".review-text") %>% html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
length(amazon_reviews)
write.table(amazon_reviews,"apple.txt",row.names = F)
getwd()


install.packages("tm")  # for text mining
install.packages(c("SnowballC","textstem")) # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes

library('tm')
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library('textstem')
# Importing apple reviews data

#apple <- read.csv("/Volumes/Data/Course Content/DS content/Text Mining/apple.txt", sep="")
# Importing apple reviews data
x <- as.character(amazon_reviews)
x <- iconv(x, "UTF-8") #Unicode Transformation Format. The '8' means it uses 8-bit blocks to represent a character

# Load the data as a corpus
x <- Corpus(VectorSource(x))
inspect(x[1])
# Convert the text to lower case
x1 <- tm_map(x, tolower)
inspect(x1[1])
# Remove numbers
x1 <- tm_map(x1, removeNumbers)
# Remove punctuations
x1 <- tm_map(x1, removePunctuation)

# Remove english common stopwords
x1 <- tm_map(x1, removeWords, stopwords('english'))
# Remove your own stop word
# specify your stopwords as a character vector
x1 <- tm_map(x1, removeWords, c("apple", "mac","the","will")) 
#striping white spaces
x1 <- tm_map(x1, stripWhitespace)
inspect(x1[1])
# Text stemming
x1<-lemmatize_words(x1)
#x1 <- tm_map(x1, stemDocument)

# Term document matrix 
# converting unstructured data to structured format using TDM

tdm <- TermDocumentMatrix(x1)
tdm <- as.matrix(tdm)
tdm
#Frequency

v <- sort(rowSums(tdm),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# Bar plot
w <- rowSums(tdm)
w_sub <- subset(w, w >= 10)
barplot(w_sub, las=3, col = rainbow(20))
# Term laptop repeats in all most all documents
x1 <- tm_map(x1, removeWords, c('apple','air',"laptop",'can','will',"amazon",'mac','macbook','product'))
x1 <- tm_map(x1, stripWhitespace)
tdm <- TermDocumentMatrix(x1)
tdm <- as.matrix(tdm)

# Bar plot
w <- rowSums(tdm)
w_sub <- subset(w, w >= 10)
barplot(w_sub, las=3, col = rainbow(20))
# Term laptop repeats in all most all documents
x1 <- tm_map(x1, removeWords, c('apple','air',"laptop",'can','will',"amazon",'mac','macbook','product'))
x1 <- tm_map(x1, stripWhitespace)
tdm <- TermDocumentMatrix(x1)
tdm <- as.matrix(tdm)
w1 <- rowSums(tdm)
# Word cloud
#with all the words

wordcloud(words = names(w1), freq = w1, 
          random.order = F, colors = rainbow(20), 
          scale=c(2,.4), rot.per = 0.3)
# lOADING +VE AND -VE dictonaries
pos.words = scan(file.choose(), what="character", comment.char=";")# read-in positive-words.txt
neg.words = scan(file.choose(), what="character", comment.char=";") # read-in negative-words.txt
pos.words = c(pos.words,"wow", "kudos", "hurray") # including our own positive words to the existing list

# Positive wordcloud
pos.matches = match(names(w), c(pos.words))
pos.matches = is.na(pos.matches)
freq_pos <- w[pos.matches]
p_names <- names(freq_pos)

wordcloud(p_names,freq_pos,scale=c(3.5,.5),colors = rainbow(20))

# Negative wordcloud
neg.matches = match(names(w), c(neg.words))
neg.matches = is.na(neg.matches)
freq_neg <- w[neg.matches]
n_names <- names(freq_neg)
wordcloud(n_names,freq_neg,scale=c(3.5,.5),colors = brewer.pal(8,"Dark2"))

#Association between words
tdm <- TermDocumentMatrix(x1)
findAssocs(tdm, c("problems"),corlimit = 0.3)

