## Topic Analysis by Matt Baker 21st July 2016
##    This takes a dataframe of raw S3 datacollector data and 
##    scrapes article text, and then does an LDA topic analysis
##    on the corpus of unique articles

## requires artdf and articledetail which is a data.frame from the JSON import out of S3, and articledetail which is a scraped list from the dd archive JSON when queried with assetID from s3 JSON

library(RTextTools)
library(topicmodels)

library(plyr)
library(tm.plugin.webmining)
library(tm)
library(ggplot2)
library(reshape2)
library(gdata)

## clear out everything except what we need
keep(articledetail,artdf,sure=TRUE)

uqIDs <- unique(artdf$asset)
# unique_article_index <- vector('list')
# for (i in 1:length(uqarts)) { #assign every index for each unique asset ID to this list so I can find later.
#   unique_article_index[[i]] <- which(artdf$asset == uqarts[i])
# }
uniquetexts <- vector()
uniqueindices <- vector()
uqkeycat <- data.frame()
for (i in 1:length(uqIDs)) {
  uniquetexts[i] <- articledetail[[which(artdf$asset == uqIDs[i])[1]]][26] # take body of the first entry of all the indexes of the unique assetIDs
  uniqueindices[i] <- which(artdf$asset == uqIDs[i])[1]
  # uqkeycat[i,] <- keycat[which(artdf$asset == uqIDs[i]),] ## right now keycat doesn't have digData for every entry
}

subset_article_HTMLstrip <- vector("character")
for (i in 1:length(uqIDs)) {
  print(i)
  if (is.null(unlist(uniquetexts[i]))) {
    print("No article text!")
    next
  }
  subset_article_HTMLstrip[i] <- extractHTMLStrip(unlist(uniquetexts[i])) # strip most of the HTML (for loop only way atm?) ## APPEARS TO BUTCHER HYPHEN-WORDS
}
uniqueplaintexts <- (gsub("\r?\n|\t", " ", subset_article_HTMLstrip)) # strip \r and \t



article_pt_corpus <- Corpus(VectorSource(uniqueplaintexts))

#start preprocessing
#Transform to lower case
article_pt_corpus <-tm_map(article_pt_corpus,content_transformer(tolower))

#remove potentially problematic symbols
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
article_pt_corpus <- tm_map(article_pt_corpus, toSpace, "-")
article_pt_corpus <- tm_map(article_pt_corpus, toSpace, "’")
article_pt_corpus <- tm_map(article_pt_corpus, toSpace, "‘")
article_pt_corpus <- tm_map(article_pt_corpus, toSpace, "•")
article_pt_corpus <- tm_map(article_pt_corpus, toSpace, "”")
article_pt_corpus <- tm_map(article_pt_corpus, toSpace, "“")

#remove punctuation
article_pt_corpus <- tm_map(article_pt_corpus, removePunctuation)
#Strip digits
article_pt_corpus <- tm_map(article_pt_corpus, removeNumbers)
#remove stopwords
article_pt_corpus <- tm_map(article_pt_corpus, removeWords, stopwords("english"))
#remove whitespace
article_pt_corpus <- tm_map(article_pt_corpus, stripWhitespace)
#Good practice to check every now and then
writeLines(as.character(article_pt_corpus[[30]]))

## below turns all words just into stems, better for topic analysis? (ie, organizing -> organiz)
#Stem document
article_pt_corpus <- tm_map(article_pt_corpus,stemDocument)

## can set custom stopwords to remove here
myStopwords <- c("can", "say","one","way","use",
                  "also","howev","tell","will",
                  "much","need","take","tend","even",
                  "like","particular","rather","said",
                  "get","well","make","ask","come","end",
                  "first","two","help","often","may",
                  "might","see","someth","thing","point",
                  "post","look","right","now","think",
                  "‘re","anoth","put","set","new","good",
                  "want","sure","kind","larg","yes,","day","etc",
                  "quit","sinc","attempt","lack","seen","awar",
                  "littl","ever","moreov","though","found","abl",
                  "enough","far","earli","away","achiev","draw",
                  "last","never","brief","bit","entir","brief",
                  "great","lot","watoday", "twitter")
article_pt_corpus <- tm_map(article_pt_corpus, removeWords, myStopwords)
#inspect a document as a check
writeLines(as.character(article_pt_corpus[[30]]))

## let's clean out corpus entries that don't have words (probalby because the original article could not be found)

#Create document-term matrix
dtm <- DocumentTermMatrix(article_pt_corpus)
rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
empty.rows <- dtm[rowTotals == 0, ]$dimnames[1][[1]]
article_pt_corpus_nonull <- article_pt_corpus[-as.numeric(empty.rows)]
dtm_nonull <- DocumentTermMatrix(article_pt_corpus_nonull)
#


#convert rownames to filenames
rownames(dtm_nonull) <- uqIDs[-as.numeric(empty.rows)]
#collapse matrix by summing over columns
freq <- colSums(as.matrix(dtm_nonull))
#length should be total number of terms
length(freq)
#create sort order (descending)
ord <- order(freq,decreasing=TRUE)
#List all terms in decreasing order of freq and write to disk
freq[ord]
write.csv(freq[ord],"word_freq_watoday.csv")

findFreqTerms(dtm_nonull,200)
wf=data.frame(term=names(freq),occurrences=freq)
library(ggplot2)
p <- ggplot(subset(wf, freq>1000), aes(term, occurrences))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

#wordcloud
library(wordcloud)
#setting the same seed each time ensures consistent look across clouds
set.seed(42)
#limit words by specifying min frequency
wordcloud(names(freq),freq, min.freq=1000)

#convert dtm to matrix
m <- as.matrix(dtm_nonull)

ptm <- proc.time()
d <- dist(m[1:1000,1:5000])
#run hierarchical clustering using Ward’s method
groups <- hclust(d,method="ward.D")
#plot dendogram, use hang to ensure that labels fall below tree
plot(groups, hang=-1)
proc.time() - ptm

#k means algorithm, 2 clusters, 100 starting configurations
kfit <- kmeans(d, 3, nstart=100)
pfit <- pam(d,3)
fanfit <- fanny(d,3)
clarafit <- clara(d,3)
#plot – need library cluster
library(cluster)
clusplot(m, kfit$cluster, color=T, shade=T, labels=2, lines=0)
clusplot(m[1:1000,1:5000],pfit,color=T, shade=T, labels=2, lines=0)
clusplot(m[1:1000,1:5000],fanfit,color=T, shade=T, labels=2, lines=0)
clusplot(m[1:1000,1:5000],clarafit,color=T, shade=T, labels=2, lines=0)

#kmeans – determine the optimum number of clusters (elbow method)
#look for “elbow” in plot of summed intra-cluster distances (withinss) as fn of k
wss <- 2:15
for (i in 2:15) wss[i] <- sum(kmeans(d,centers=i,nstart=25)$withinss)
plot(2:5, wss[2:15], type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")


#Set parameters for Gibbs sampling
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE
#Number of topics
k <- 5
#Run LDA using Gibbs sampling

#write out results
#docs to topics
ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics.csv"))


#top 6 terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut,6))
write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicsToTerms.csv"))


#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilities.csv"))


#Find relative importance of top 2 topics
topic1ToTopic2 <- lapply(1:nrow(dtm_nonull),function(x)
  sort(topicProbabilities[x,])[k]/sort(topicProbabilities[x,])[k-1])


#Find relative importance of second and third most important topics
topic2ToTopic3 <- lapply(1:nrow(dtm_nonull),function(x)
  sort(topicProbabilities[x,])[k-1]/sort(topicProbabilities[x,])[k-2])


#write to file
write.csv(topic1ToTopic2,file=paste("LDAGibbs",k,"Topic1ToTopic2.csv"))
write.csv(topic2ToTopic3,file=paste("LDAGibbs",k,"Topic2ToTopic3.csv"))
