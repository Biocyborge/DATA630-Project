####testing####

Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", 
            "cluster", "igraph", "fpc")
install.packages(Needed, dependencies = TRUE)

install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")

setwd("C:/Users/Admin-PC/Desktop/DATA630/Group Project")


####READ THE FILES INTO A FRAME####
#each column has to be labeled
#after that the

#1
Andy.Weir.The.Martian<- read.delim("Andy-Weir-The-Martian.csv", header = FALSE)
colnames(Andy.Weir.The.Martian)<- c("Score", "URL", "Review Title", "review text")
Andy.Weir.The.Martian$URL<- NULL
Andy.Weir.The.Martian$Score<-factor(Andy.Weir.The.Martian$Score)


#2
EL.James.Fifty.Shades.of.Grey<- read.delim("EL-James-Fifty-Shades-of-Grey.csv", header = FALSE)
colnames(EL.James.Fifty.Shades.of.Grey)<- c("Score", "URL", "Review Title", "review text")
EL.James.Fifty.Shades.of.Grey$URL<- NULL
EL.James.Fifty.Shades.of.Grey$Score<-factor(EL.James.Fifty.Shades.of.Grey$Score)


#3
Laura.Hillenbrand.Unbroken<- read.delim("Laura-Hillenbrand-Unbroken.csv", header = FALSE)
colnames(Laura.Hillenbrand.Unbroken)<- c("Score", "URL", "Review Title", "review text")
Laura.Hillenbrand.Unbroken$URL<- NULL
Laura.Hillenbrand.Unbroken$Score<-factor(Laura.Hillenbrand.Unbroken$Score)


#4
Donna.Tartt.The.Goldfinch<- read.delim("Donna-Tartt-The-Goldfinch.csv", header = FALSE)
colnames(Donna.Tartt.The.Goldfinch)<- c("Score", "URL", "Review Title", "review text")
Donna.Tartt.The.Goldfinch$URL<-NULL
Donna.Tartt.The.Goldfinch$Score<-factor(Donna.Tartt.The.Goldfinch$Score)


#5
Fillian.Flynn.Gone.Girl<- read.delim("Fillian_Flynn-Gone_Girl.csv", header = FALSE)
colnames(Fillian.Flynn.Gone.Girl)<- c("Score", "URL", "Review Title", "review text")
Fillian.Flynn.Gone.Girl$URL<-NULL
Fillian.Flynn.Gone.Girl$Score<-factor(Fillian.Flynn.Gone.Girl$Score)


#6
John.Green.The.Fault.in.our.Stars<- read.delim("John-Green-The-Fault-in-our-Stars.csv", header = FALSE)
colnames(John.Green.The.Fault.in.our.Stars)<- c("Score", "URL", "Review Title", "review text")
John.Green.The.Fault.in.our.Stars$URL<-NULL
John.Green.The.Fault.in.our.Stars$Score<-factor(John.Green.The.Fault.in.our.Stars$Score)


#7
Paula.Hawkins.The.Girl.On.The.Train<- read.delim("Paula_Hawkins-The-Girl-On-The-Train.csv", header = FALSE)
colnames(Paula.Hawkins.The.Girl.On.The.Train)<- c("Score", "URL", "Review Title", "review text")
Paula.Hawkins.The.Girl.On.The.Train$URL<-NULL
Paula.Hawkins.The.Girl.On.The.Train$Score<-factor(Paula.Hawkins.The.Girl.On.The.Train$Score)


#8
Suzanne.Collins.The.Hunger.Games<- read.delim("Suzanne-Collins-The-Hunger-Games.csv", header = FALSE)
colnames(Suzanne.Collins.The.Hunger.Games)<- c("Score", "URL", "Review Title", "review text")
Suzanne.Collins.The.Hunger.Games$URL<-NULL
Suzanne.Collins.The.Hunger.Games$Score<-factor(Suzanne.Collins.The.Hunger.Games$Score)


#VCorpus only accepts files from a directory so they will need to be exported to a new set of files and read into the function

write.csv(Andy.Weir.The.Martian,"C:\\Users\\Admin-PC\\Desktop\\DATA630\\Group Project//labled datasets\\Andy.Weir.The.Martian.csv", row.names=FALSE)
write.csv(EL.James.Fifty.Shades.of.Grey,"C:\\Users\\Admin-PC\\Desktop\\DATA630\\Group Project//labled datasets\\EL.James.Fifty.Shades.of.Grey.csv", row.names=FALSE)
write.csv(Laura.Hillenbrand.Unbroken,"C:\\Users\\Admin-PC\\Desktop\\DATA630\\Group Project//labled datasets\\Laura.Hillenbrand.Unbroken.csv", row.names=FALSE)
write.csv(Donna.Tartt.The.Goldfinch,"C:\\Users\\Admin-PC\\Desktop\\DATA630\\Group Project//labled datasets\\Donna.Tartt.The.Goldfinch.csv", row.names=FALSE)
write.csv(Fillian.Flynn.Gone.Girl,"C:\\Users\\Admin-PC\\Desktop\\DATA630\\Group Project//labled datasets\\Fillian.Flynn.Gone.Girl.csv", row.names=FALSE)
write.csv(John.Green.The.Fault.in.our.Stars,"C:\\Users\\Admin-PC\\Desktop\\DATA630\\Group Project//labled datasets\\John.Green.The.Fault.in.our.Stars.csv", row.names=FALSE)
write.csv(Paula.Hawkins.The.Girl.On.The.Train,"C:\\Users\\Admin-PC\\Desktop\\DATA630\\Group Project//labled datasets\\Paula.Hawkins.The.Girl.On.The.Train.csv", row.names=FALSE)
write.csv(Suzanne.Collins.The.Hunger.Games,"C:\\Users\\Admin-PC\\Desktop\\DATA630\\Group Project//labled datasets\\Suzanne.Collins.The.Hunger.Games.csv", row.names=FALSE)

#plot the distribution of reviews
all_books <- bind_rows(Andy.Weir.The.Martian = Andy.Weir.The.Martian, 
                       Donna.Tartt.The.Goldfinch = Donna.Tartt.The.Goldfinch, 
                       EL.James.Fifty.Shades.of.Grey = EL.James.Fifty.Shades.of.Grey,
                       Fillian.Flynn.Gone.Girl = Fillian.Flynn.Gone.Girl, 
                       John.Green.The.Fault.in.our.Stars = John.Green.The.Fault.in.our.Stars, 
                       Laura.Hillenbrand.Unbroken = Laura.Hillenbrand.Unbroken,
                       Paula.Hawkins.The.Girl.On.The.Train = Paula.Hawkins.The.Girl.On.The.Train, 
                       Suzanne.Collins.The.Hunger.Games = Suzanne.Collins.The.Hunger.Games, 
                       .id = 'Book')

ggplot(all_books, aes(fill = Book, x = Score))+
  geom_bar(position = "dodge", stat = "count")+
  ylab("Number of Reviews")+
  xlab("Stars Given")+
  ggtitle("Distribution of Reviews")


####Text Analysis####
library(tm)
library(caTools)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(BiocManager)
library(Rgraphviz)
cname<- file.path("C:/Users/Admin-PC/Desktop/DATA630/Group Project/labled datasets")
cname
dir(cname)
#1 Pre-processing
#create stop words vectors
default_stopwords <- stopwords("english")
custom_stopwords <- c("read", "reading", "book", "books", "classasizebase", "reviewtexti", "span", "reviewtextthis")
all_stopwords <- c(default_stopwords, custom_stopwords)

#remove stop words,punctuation and numbers
corpus_books<-Corpus(DirSource(cname))
corpus_books <- tm_map(corpus_books, tolower)
corpus_books <- tm_map(corpus_books, removePunctuation)
corpus_books <- tm_map(corpus_books, removeNumbers)
corpus_books <- tm_map(corpus_books, removeWords, all_stopwords)
corpus_books <- tm_map(corpus_books, stripWhitespace)
corpus_books <- tm_map(corpus_books, stemDocument)
dtm$cluster
dtm_books<-DocumentTermMatrix(corpus_books[2])
#remove sparse terms
dtm_books<-removeSparseTerms(dtm_books, 0.2)

wf <- data.frame(docs=Docs(dtm_books), as.matrix(dtm_books))

wf<- wf %>% gather(key = "word", value = "n", -docs)

wf2<- wf[order(wf$n,decreasing = TRUE),] %>%
  top_n(50)


  ggplot(wf2, aes( x = reorder(word, -n), y =n)) + 
  geom_bar(stat="identity") +
  facet_wrap(~ docs) + 
  theme(axis.text.x=element_text(angle=45, hjust=1))

#term document matrices with TF-IDF Weight
tdm_books<-TermDocumentMatrix(corpus_books, control = list(weighting = weightTfIdf))
inspect(tdm_books)
freq = rowSums(as.matrix(tdm_books))
head(freq,10)
tail(freq,10)
tail(sort(freq), n=20)

top20.freq = tail(sort(freq), n = 20)
top20.freq.df=as.data.frame(sort(top20.freq))
top20.freq.df$names <- rownames(top20.freq.df)

ggplot(top20.freq.df, aes(reorder(names,top20.freq), top20.freq)) +
  geom_bar(stat="identity") + coord_flip() + 
  xlab("Terms") + ylab("Frequency") +
  ggtitle("Term frequencies")

#term document matrices with TF-IDF Weight
dtm_books2<-DocumentTermMatrix(corpus_books, control = list(weighting = weightTfIdf))
inspect(dtm_books2)
freq2 = rowSums(as.matrix(dtm_books2))
head(freq,10)
tail(freq,10)
tail(sort(freq), n=20)

top20.freq = tail(sort(freq), n = 20)
top20.freq.df=as.data.frame(sort(top20.freq))
top20.freq.df$names <- rownames(top20.freq.df)

ggplot(top20.freq.df, aes(reorder(names,top20.freq), top20.freq)) +
  geom_bar(stat="identity") + coord_flip() + 
  xlab("Terms") + ylab("Frequency") +
  ggtitle("Term frequencies")
