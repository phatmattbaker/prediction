library(jsonlite)

smhdf <- data.frame(fromJSON("~/Documents/NewsLabs/prediction/ForMatt_S3files_raw/smh/smh_05-2016_allcorrect.json"))

### Looking at correlated sources

smhrefdev <- table(smhdf$normalisedReferer,smhdf$deviceType)
# # pull out all internals:
# smhsmhrefdev[grep("referer:internal*",rownames(smhrefdev)),]
# # pull out all socials:
# smhrefdev[grep("referer:social*",rownames(smhrefdev)),]
# let's make this simpler, we just want search, internal, 
social_bydev <- colSums(smhrefdev[grep("referer:social*",rownames(smhrefdev)),])
internal_bydev <- colSums(smhrefdev[grep("referer:internal*",rownames(smhrefdev)),])
search_bydev <- colSums(smhrefdev[grep("referer:search:*",rownames(smhrefdev)),])
direct_bydev <- smhrefdev[grep("referer:direct*",rownames(smhrefdev)),] # no need to sum because only one direct
# we want to make this into a new table ideally?:
summary_device <- t(data.frame(direct_bydev,internal_bydev,search_bydev,social_bydev))
# but a smarter R user would do this all at the giant df level, so you create subdfs of each type, then you can keep and scrape the articles also:
important_referers <- c("referer:internal:smh", "referer:social:facebook", "referer:search:google")
important_bydev <- smhrefdev[important_referers,]
mosaicplot(smhrefdev[important_referers,1:3], main = "Proportion of Device by Referrer", color = TRUE,cex.axis = 1)
mosaicplot(summary_device[,1:3],main = "Proportion of Device by Referrer", color = TRUE,cex.axis = 1)
proportion <- important_bydev[important_referers,c("desktop","mobile")]/rowSums(important_bydev[important_referers,])
