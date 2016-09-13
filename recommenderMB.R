# goal: extract recommendations

# inputs
# articles
# recommend
# recommend_default

# outputs
# recommends()

library(dplyr)

# find associations, fill rest with popular articles in same category
# the line below is not a comment. It is an instruction for the HTTP API from the plumber library
#* @get /api/v0/recommends
recommends <- function(page_id, brand) {
  
  # for assessing results
  # print(articles[articles$page_id==page_id, c("page_id","n_users","url")])
  
  # for consodle monitoring
  # print(paste(Sys.time(), page_id, sep=",") )
  print(paste(Sys.time(), page_id, extract_title(as.character(articles[articles$page_id==page_id, c("url")])), sep=", ") )
  
  assoc = data.frame(recommend[recommend$lhs==page_id, c("page_id")])
  
  # if article not found, return 5 most popular articles for that brand
  if (nrow(assoc) == 0 ) { 
    return (
      filter(articles, brand == brand) %>%
        top_n(5, pv) %>% 
        arrange( desc(pv)) %>% 
        select(title, url)
    )
  }
  
  names(assoc) = c("page_id")
  assoc = merge(assoc, articles[articles$brand == brand,c("page_id","url","category","title")], sort = FALSE)
  
  #if url not found for a brand, use default brand or pick first record
  # note: if pv are only sourced from watoday, this is not required
  
  # put items from same category on top of list
  category = as.character( articles[articles$page_id == page_id, "category"][[1]] )
  assoc$same_category = (assoc$category == category)
  assoc = assoc[order(assoc$same_category, decreasing = TRUE), c("title", "url")]
  
  if (nrow(assoc) >= 5) { return(assoc[1:5, ]) }
  
  # Fill rest of recommendations with popular items
  pop = recommend_default[recommend_default$category==category, ]
  nrows = 5 - nrow(assoc)
  pop = pop[1:nrows, c("title", "url")]
  
  rbind(assoc, pop)
}


# page not fouhd: default recommendation
recommends(1, "watoday")
# recommends(1013308657, "watoday")

print( paste("Refresh end:", Sys.time(), "----------------------------------------------------------------------------------------------------------------") )
print("")
