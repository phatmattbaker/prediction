# # testing corpus building on 1000 articles from SMH collection 18/7/2016
library(plyr)
library(tm.plugin.webmining)
library(tm)
library(ggplot2)
library(reshape2)

# uqart <- unique(unlist(assetIDs))

uqarts <- unique(artdf$asset)
# unique_article_index <- vector('list')
# for (i in 1:length(uqarts)) { #assign every index for each unique asset ID to this list so I can find later.
#   unique_article_index[[i]] <- which(artdf$asset == uqarts[i])
# }
uniquetexts <- vector()
for (i in 1:length(uqarts)) {
  uniquetexts[i] <- articledetail[[which(artdf$asset == uqarts[i])[1]]][26] # take body of the first entry of all the indexes of the unique assetIDs
}


number_of_articles = 359
subset_article_HTMLstrip <- vector("character")

subset_article_text <- lapply(articledetail[1:number_of_articles], function(x) x[c(2,26)]) # this is taking assetID and article now, as I want to strip later for uniques

# sadf <- ldply(sadt, function(x) x[[1]])

for (i in 1:number_of_articles) {
  print(i)
  if (is.null(unlist(subset_article_text[i]))) {
    print("No article text!")
    next
  }
  subset_article_HTMLstrip[i] <- extractHTMLStrip(unlist(subset_article_text[i])) # strip most of the HTML (for loop only way atm?) ## APPEARS TO BUTCHER HYPHEN-WORDS
}
subset_article_plaintext <- (gsub("\r?\n|\t", " ", subset_article_HTMLstrip)) # strip \r and \t

article_pt_corpus <- Corpus(VectorSource(unlist(subset_article_plaintext)))
tdm <- DocumentTermMatrix(article_pt_corpus, control = list(removePunctuation = TRUE, stopwords=TRUE))

findFreqTerms(tdm,5)
tdm.common = removeSparseTerms(tdm, 0.8)
dim(tdm.common)
inspect(tdm.common)

tdm.dense <-as.matrix(tdm.common)
order.tdm.dense <- tdm.dense[order(rownames(tdm.dense)),]
heatmap <- heatmap(order.tdm.dense[1:40,], Rowv=NA, Colv=NA, col = cm.colors(256), scale="column", margins=c(5,10))
heatmap <- heatmap(order.tdm.dense[which(rownames(order.tdm.dense) == "entertainment"),], Rowv=NA, Colv=NA, col = cm.colors(256), scale="column", margins=c(5,10))


order.tdm.dense[which(rownames(order.tdm.dense) == "entertainment"),]

# lets link asset names to row names on corpus
assetIDs <- lapply(articledetail[1:number_of_articles], function(x) (x[2]))
assetIDs <- unlist(assetIDs)
rownames(tdm.dense) = assetIDs
tdmdf <- melt(tdm.dense, value.name="count")
# tdmdf$Docs <- as.character(tdmdf$Docs) ## appears heatmap works, just ggplot needs it?

# we now have a data frame based on text frequency above sparsity of 80%

heatmap <- heatmap(tdm.dense, Rowv=NA, Colv=NA, col = cm.colors(256), scale="column", margins=c(5,10))


# # I don't get it, but ggplot2 shits itself with character based integer headings.
# qplot(Docs,Terms,fill=count,geom="tile",data=tdmdf)
# 
# ggplot() + #tdmdf, aes(x = Docs, y = Terms, fill = count)) +
#   geom_tile(data = tdmdf, aes(x = Docs, y = Terms, fill = count), colour = "white") 


# ssadt <- lapply(sadt[1:10],cat(extractHTMLStrip(unlist())))
# sadt <- lapply(sadt, cat(extractHTMLStrip()))
