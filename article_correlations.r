# goal: generate recommendations for a page 
# method: calculate correlations on user/page matrix

# inputs
# pviews

# outputs
# simil
# find_similar_arts()



maxRow <- dim(pviews)[1] # full pviews for now
page_user <- data.frame(member_id=pviews$member_id[1:maxRow],page_id=pviews$page_id[1:maxRow])
print(system.time(
  counts_page_user <- table(page_user) ##MB: perhaps this is crazy slow way to do this? Am sure dplyr offers better
))
##    user  system elapsed 
##  3.343   2.096   5.438 ## this is timings for the full table creation, so not so slow

print(system.time(
  simil <- cor(counts_page_user)
))
# takes about 5 minutes for 1.5 m rows

calc_correlations = function(pviews) {
  maxRow <- dim(pviews)[1] # full pviews for now
  page_user <- data.frame(member_id=pviews$member_id[1:maxRow],page_id=pviews$page_id[1:maxRow])
  counts_page_user <- table(page_user) ##MB: perhaps this is crazy slow way to do this? Am sure dplyr offers better

  cor(counts_page_user)
}

print(system.time(
  simil <- calc_correlations(pviews)
))

saveRDS(simil, 'simil.rds')


find_similar_arts <- function(page_id, simil, n=5) {
  similar <- simil[,page_id]
  similar <- similar[order(-similar)]
  # n <- min(n, nrow(similar))
  recommends <- names(similar[2:(n+1)]) # because 1 is just same article!
  recommends
}

## test for a single page_id
find_similar_arts(rownames(simil)[1], simil)
#####################################

# view results
assess_results = function() {
  for (x in pages_today$page_id) {
    
    r1 = find_similar_arts(as.character(x), simil)
    df = as.data.frame(r1)
    names(df) = "page_id"
    r2 = merge(df, pages[,c("page_id","url")], sort = FALSE)
    
    r2$url = sapply(r2$url, extract_title)
    print(pages[pages$page_id==x, c("page_id","n_users","url")])
    print(r2)
    
    cat ("Press x to exit, any other key to see next item")
    line <- readline()
    if (line == "x") { break } 
    
  }
}

