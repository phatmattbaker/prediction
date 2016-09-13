## Matt Baker 26th July 2016. 
## This file takes in JSON out of rewire from s3:///ffx-recs-results/am-pm/smh/e0-rewire-a/
## and sorts based on number of recommends across entire userbase, number of views across history, and highest scoring, and then aggregates the uniques among those.

library(plyr)
library(jsonlite)
library(plyr)
library(stringr)
# library(magrittr)
library(rvest)
# library(httr)
library(tm.plugin.webmining)
library(ggplot2)
library(reshape2)

filelist <- dir(pattern = "json")
recs <- vector('list')
history <- vector('list')
userID <- vector()
for (i in 1:length(filelist)) {
  tempJSON <- fromJSON(filelist[i])
  history[[i]] <- tempJSON$history
  recs[[i]] <- tempJSON$recs
  userID[i] <- tempJSON$id
}
recdf <- ldply(recs)
historydf <- ldply(history)

recommended_arts <- as.data.frame(table(recdf$id))
sortrecarts <- recommended_arts[with(recommended_arts, order(-Freq, Var1)),] ## don't understand exactly what with does, but it works

wantedlimit <- 50

## assemble top 30 most recommended articles
toprecdf <- data.frame()
for (i in 1:wantedlimit) {
  toprecdf <-rbind(toprecdf,recdf[recdf$id==sortrecarts[i,1],][1,])
}
# mostrec <- recdf[which(recommended_arts[,2]==max(recommended_arts[,2])),]

## assemble top 30 most viewed articles in history
history_arts <- as.data.frame(table(historydf$id))
sorthistarts <- history_arts[with(history_arts, order(-Freq, Var1)),] ## don't understand exactly what with does, but it works
## assemble top 30 most recommended articles
tophistdf <- data.frame()
for (i in 1:wantedlimit) {
  tophistdf <-rbind(tophistdf,historydf[historydf$id==sorthistarts[i,1],][1,])
}
# mosthist <- historydf[which(history_arts[,2]==max(history_arts[,2])),]

## assemble top 30 highest scored recommends

sortrecdf <- recdf[with(recdf, order(-score, id)),]
topscoredf <- sortrecdf[1:wantedlimit,]

## unique articles among all 3 top 30s:
uqrecIDs <- unique(c(tophistdf$id,topscoredf$id,toprecdf$id))
uqrecdf <- data.frame()
for (i in 1:length(uqrecIDs)) {
  if (sum(recdf$id == uqrecIDs[i]) == 0) {
    uqrecdf <- rbind(uqrecdf, historydf[historydf$id==uqrecIDs[i],1:6][1,])
  } else {
    uqrecdf <- rbind(uqrecdf, recdf[recdf$id==uqrecIDs[i],1:6][1,])
  }
}

## below makes a corpus of all the unique recommends across this set.

uqartdetail <- vector('list')
assetbaseurl = "http://ddj.smh.com.au/interactive/shared/tpc/c.php?aid="
# above uses Andy's direct CQ interface to grab articles
for (i in 1:length(uqrecIDs)) {
  print(i)
  thisurl = paste(assetbaseurl,uqrecIDs[i],sep = "")
  uqartdetail[[i]] <- tryCatch(fromJSON(thisurl), error=function(e) NULL) # if no article there, just forget it (write null to that one)
}

uniquetexts <- vector()
for (i in 1:length(uqrecIDs)) {
  uniquetexts[i] <- uqartdetail[[i]][26] # take body of the first entry of all the indexes of the unique assetIDs
}

subset_article_HTMLstrip <- vector("character")
for (i in 1:length(uqrecIDs)) {
  print(i)
  if (is.null(unlist(uniquetexts[i]))) {
    print("No article text!")
    next
  }
  subset_article_HTMLstrip[i] <- extractHTMLStrip(unlist(uniquetexts[i])) # strip most of the HTML (for loop only way atm?) ## APPEARS TO BUTCHER HYPHEN-WORDS
}
uniqueplaintexts <- (gsub("\r?\n|\t", " ", subset_article_HTMLstrip)) # strip \r and \t

## take these unique texts and process as corpora:

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
                 "great","lot")
article_pt_corpus <- tm_map(article_pt_corpus, removeWords, myStopwords)
#inspect a document as a check
writeLines(as.character(article_pt_corpus[[30]]))

#Create document-term matrix
dtm <- DocumentTermMatrix(article_pt_corpus)
#


#convert rownames to filenames
rownames(dtm) <- uqrecIDs
#collapse matrix by summing over columns
freq <- colSums(as.matrix(dtm))
#length should be total number of terms
length(freq)
#create sort order (descending)
ord <- order(freq,decreasing=TRUE)
#List all terms in decreasing order of freq and write to disk
freq[ord]
saveStr <- paste(as.character(uqartdetail[[1]]$creationTime),"_wordFreq.csv")
saveStr <- gsub(" ", "", saveStr)
saveStr <- gsub(":","",saveStr, fixed=TRUE)
write.csv(freq[ord],saveStr)

findFreqTerms(dtm_nonull,50)
wf=data.frame(term=names(freq),occurrences=freq)
p <- ggplot(subset(wf, freq>100), aes(term, occurrences))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

## clustering based on recommended articles:

#wordcloud
library(wordcloud)
#setting the same seed each time ensures consistent look across clouds
set.seed(42)
#limit words by specifying min frequency
wordcloud(names(freq),freq, min.freq=50)

#convert dtm to matrix
m <- as.matrix(dtm)

ptm <- proc.time()
d <- dist(m)
#run hierarchical clustering using Ward’s method
groups <- hclust(d,method="ward.D")
#plot dendogram, use hang to ensure that labels fall below tree
plot(groups, hang=-1)
proc.time() - ptm

# #k means algorithm, 2 clusters, 100 starting configurations
kfit <- kmeans(d, 5, nstart=100)
library(cluster)
pfit <- pam(d,5)
# fanfit <- fanny(d,5)
# clarafit <- clara(d,5)
#plot – need library cluster
# clusplot(m, kfit$cluster, color=T, shade=T, labels=2, lines=0)
clusplot(pfit, color=T, shade=T, labels=2, lines=0)

library(RTextTools)
library(topicmodels)
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE
#Number of topics
k <- 5
#Run LDA using Gibbs sampling
ldaOutrec <- vector('list')
for (i in 3:9) {
  ldaOutrec[[i]] <-LDA(dtm,i, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
}



### old code to make histograms.

# breaks <- seq(from = 5, to = 900, by = 5)
# hist(recommended_arts[,2], breaks = breaks)
# hist(history_arts[,2], breaks = seq(from = 0, to = (max(history_arts[,2]) + 10), by = 5), ylim = c(0,200))
# 
# highscoringarts <- recdf$id[recdf$score>200]
# hsartdf <- recdf[recdf$score>200,] ## interestingly these are all election pieces
# maxart <- recdf[which.max(recdf$score),]
# 

# 
# require(vcd)
# require(MASS)
# 
# fit1 <- fitdistr(history_arts[,2], "exponential") 
# 
# # goodness of fit test
# ks.test(history_arts[,2], "pexp", fit1$estimate) # p-value > 0.05 -> distribution not refused
# 
# # plot a graph
# hist(history_arts[,2], freq = FALSE, breaks = 100, xlim = c(0, quantile(history_arts[,2], 0.99)))
# curve(dexp(x, rate = fit1$estimate), col = "red", add = TRUE)
# 
# 
