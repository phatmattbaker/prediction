# # Take a text file out of S3 that has had a comma added between each line, using VIM, and a [] added around ends.
# # JSON MUST HAVE NO TRAILING COMMA!
# # using vim top do this atm: :%s/$/,/

library(jsonlite)
library(plyr)
library(stringr)
library(magrittr)
library(rvest)

#### TAKE IN JSON CLEANED DATA FROM S3 AMAZON
# longdf <- data.frame(fromJSON("~/Documents/NewsLabs/prediction/ForMatt_S3files_raw/ctimes/05-2016-ctimes-view-DCDS-"))

# longdf <- data.frame(fromJSON("~/Documents/NewsLabs/prediction/ForMatt_S3files_raw/watoday/watoday_clean.json"))
# longdf <- data.frame(fromJSON(readLines("/Users/bakerm/Documents/NewsLabs/prediction/ForMatt_S3files_raw/watoday/watoday_clean.json")))
## using rjsonlite
wajson <- (fromJSON(readLines("/Users/bakerm/Documents/NewsLabs/prediction/ForMatt_S3files_raw/watoday/watoday_clean.json")))
longdf <- data.frame(wajson)

# bjson <- fromJSON(readLines("/Users/bakerm/Documents/NewsLabs/prediction/ForMatt_S3files_raw/btimes/07-2016-btimes-view-DCDS-1"))
# longdf <- data.frame(bjson)

# ctimesjson <- fromJSON(readLines("~/Documents/NewsLabs/prediction/ForMatt_S3files_raw/ctimes/05-2016-ctimes-view-DCDS-"))
# longdf <- data.frame(ctimesjson)

# cutdf <- longdf
# cutdf$extras <- NULL

# cutdf <- cutdf[!cutdf$assetType=="Homepage"]
# fbdf <- cutdf[cutdf$normalisedReferer=="referer:social:facebook",]
# 

## strip df down to uniques only, as we are doing tm analysis on this
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


#### BELOW IS FOR SCRAPING FULL ARTICLE TEXT, GIVEN A DATA FRAME FULL OF ASSETIDS AND URLS




assetbaseurl = "http://ddj.smh.com.au/interactive/shared/tpc/c.php?aid="
# above uses Andy's direct CQ interface to grab articles

# # wtf is going on with assetIDs?
notsocdf <- longdf[-grep("referer:social*",longdf$normalisedReferer),]
artdf <- longdf[longdf$assetType=="Article",]

## below scrapes article text from Andy's CQ interface... but WE SHOULD ONLY SCRAPE UNIQUES!
articledetail <- vector('list') ## for now I am going to make this a big list, see if I can flip it into data.frame later.
for (i in 1:nrow(artdf)) {
  print(i)
  thisurl = paste(assetbaseurl,artdf$asset[i],sep = "")
  articledetail[[i]] <- tryCatch(fromJSON(thisurl), error=function(e) NULL) # if no article there, just forget it (write null to that one)
  # fbdf[i,"body"] <- articledetail[[i]]$body
  # fbdf[i,"keywords"] <- articledetail[[i]]$keywords
  # fbdf[i,"byline"] <- articledetail[[i]]$byline
}


#### BELOW IS FOR SCRAPING TITAN AD DATA (ADCAT) (is this just the same as cropping from URL?)

keycat <- matrix(ncol = 2, nrow = dim(artdf)[1]) #predeclare for safety and to avoid it shitting itself
## scrape based on article URL and digitalData, if existing, to get keywords
for (i in 1:length(uniqueindices)) { #uniqueindices defined in TopicAnalysis.R
  print(i)
  arturl <- artdf$url[uniqueindices[i]]
  if (is.na(arturl)) {
    print("no url!")
    next
  } # bail on loop if there's no URL!
  tempdigData <- arturl %>% read_html() %>% html_node(xpath="//script") %>% html_text() # extracts the var = digitalData script segment
  tt2 <- strsplit(tempdigData,"\n") # splits by end of line to get just the JSON
  digDatStr <- tt2[[1]][3] # just take the line that starts var = digitalData
  if (is.null(digDatStr)) {
    print("null digDatStr")
    next
  } # some of these are formatted differently, let's just skip for now, otherwise we can come back and grep
  ddss <- strsplit(digDatStr,"= ") # get rid of var = digitalData, just look at the rest
  nearlyJSON <- ddss[[1]][2] # fix up the JSON, add [ at start, ] at end, and remove semicolon
  # nearlyJSON <- sub(";","]",nearlyJSON, fixed=TRUE)
  nearlyJSON <- gsub("[;]+$","]",nearlyJSON) # doing it with regexp instead, safer [remove last semicolon]
  nearlyJSON <- paste("[",nearlyJSON,sep = "")
  tj <- fromJSON(nearlyJSON)
  keys <- as.matrix(unlist(tj[[2]][3])) # we are being careful here because some titan entries don't have 4 entries, only 3!
  keycat[i,1] <- keys["adKeyValues.cat",] # first column is broad category
  if (dim(keys)[1] == 4) { # in case there isn't a keyword!
    keycat[i,2] <- keys["adKeyValues.cat1",] # second column is more focused category
  }
}


articledetaildf <- data.frame(t(sapply(articledetail,c))) # don't full understand this, but chops a list into a dataframe and preserves headings

# it is currently chopping up relatedAssets which has 'related articles' and may be useful to build a network map of related articles, or, simply put, if social user goes for an article, display related article straight away as more likely to read more on it?

simpartdetdf <- articledetaildf
simpartdetdf$relatedAssets <- NULL # I'm discarding this for now but will work out tomorrow (15/7/16) how to keep as list within data.table

mergefbart <- data.frame(fbdf,simpartdetdf)  # # currently this is failing also.

### Looking at correlated sources 15/7/17

refdev <- table(longdf$normalisedReferer,longdf$deviceType)
# pull out all internals:
refdev[grep("referer:internal*",rownames(refdev)),]
# pull out all socials:
refdev[grep("referer:social*",rownames(refdev)),]
# let's make this simpler, we just want search, internal, 
social_bydev <- colSums(refdev[grep("referer:social*",rownames(refdev)),])
internal_bydev <- colSums(refdev[grep("referer:internal*",rownames(refdev)),])
search_bydev <- colSums(refdev[grep("referer:search:*",rownames(refdev)),])
direct_bydev <- refdev[grep("referer:direct*",rownames(refdev)),] # no need to sum because only one direct
# we want to make this into a new table ideally?:
summary_device <- t(data.frame(direct_bydev,internal_bydev,search_bydev,social_bydev))
# but a smarter R user would do this all at the giant df level, so you create subdfs of each type, then you can keep and scrape the articles also:
important_referers <- c("referer:internal:watoday", "referer:social:facebook", "referer:search:google")
important_bydev <- refdev[important_referers,]
mosaicplot(refdev[important_referers,2:4], main = "watoday: Proportion of Device by Referrer", color = TRUE,cex.axis = 1)
mosaicplot(summary_device[,1:3],main = "watoday: Proportion of Device by Referrer", color = TRUE,cex.axis = 1)