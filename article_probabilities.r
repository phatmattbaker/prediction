# goal: generate recommendations for a page 
# method: conditional probability
# p(y/x) = ( p(x&y) / p(x) ) / ( p(!x&y) / p(!x) )

# inputs
# pviews

# outputs
# pred
# recommend_article()


library(dplyr)

setwd("/usr/share/SourceCode/recommender/article")
pviews = readRDS('pviews_members.rds')

# calc latest date 
DAY = max(pviews$creation_date,na.rm=T)
# the goal is to calculate prediction for today's pages only.
# yesterday's preditions were calculated in yesterday's run.
# 31% pviews from articles created today
# 59% pviees from articles created yesterday
# 3% pviews from articles created 2 days ago

# 1.5m pviews start with.

# pviews = pviews[,c("member_id","page_id")]
pviews = tbl_df(pviews) # important for dplyr to be fast

# 117,000 members
length(unique(pviews$member_id))

# remove users than have only viewed one page.
# 101,000 members have looked at more than one page
members = group_by(pviews, member_id) %>%
  summarise(n_entries = n_distinct(page_id)) %>%
  filter(n_entries > 1)

# 1.484m page views left.
pviews = merge(pviews, members[,"member_id"]) 

# 13,000 pages
pages = group_by(pviews, page_id) %>%
  summarise(n_users = n_distinct(member_id)) 

# filter out pages with less than 10 viewers
# 2,300 pages left
pages = group_by(pviews, page_id) %>%
  summarise(n_users = n_distinct(member_id)) %>%
  filter(n_users > 9)

# add url & created_date to pages 
# note: many pages have several URLs (smh, age, ctimes, ...) & several page titles
pages_all = group_by(pviews, page_id) %>%
  summarise(url = first(page_url)) %>%
  select(page_id, url) %>%
  distinct()
pages = merge(pages, pages_all)
rm(pages_all)


# 1.47m page views left
pviews = merge(pviews, pages[,"page_id"]) 

# filter out members who have not seen a page created today. They can't be useful for recommendations on DAY
# about 30% of pviews filtered out.
members = group_by(pviews, member_id) %>%
  filter(creation_date == DAY) %>%
  select(member_id) %>%
  distinct()

# 1.469m page views left
pviews = merge(pviews, members[,"member_id"]) 

# 100,000 members who looked at more than 1 page
members = group_by(pviews, member_id) %>%
  summarise(n_entries = n_distinct(page_id)) %>%
  filter(n_entries > 1)

# 1.469m page views left
pviews = merge(pviews, members[,"member_id"]) 

# recalc pages
pages = group_by(pviews, page_id) %>%
  summarise(n_users = n_distinct(member_id))

# p(x) = pages(x) / # members
# p(!x) = 1 - p(x)

# p(x&y) = # members who have x & y  / # members
# p(!x&y) = p(y) - p(x&y)

# need to re-shape the data to get a list of pages per visitor.
# essentially, need a serialise a vector into a string

# p(y/x) = ( p(x&y) / p(x) ) / ( p(!x&y) / p(!x) )
# pviews[pviews$member_id=="00000035304126904543503762274746446058","page_id"]


# serialise a vector into a comma-delimited string
serialise = function(x) {
 paste(",", paste(x, collapse=","), ",", sep="")
}

members_pages = group_by(pviews, member_id) %>% 
  do(pages = serialise(.$page_id))

members = merge(members, members_pages)
rm(members_pages)

# as.character(members[members$member_id=="00005857573747098403549756282261047998","pages"])

# p(x)
# pages[pages$page_id==1010003787, "n_users"]

# pages viewed together
# p(x&y)
# as.integer( filter(members, grepl(",1011798701,", pages) & grepl(",1011785042,", pages)) %>% 
#   select(pages) %>% 
#   mutate(p = as.character(pages)) %>%
#   count() )

# print(system.time(
#   filter(members, grepl(",1011798701,", pages) ) %>% 
#   filter(grepl(",1011785042,", pages)) %>%
#   count()
# ))

pages = merge(pages, unique(pviews[,c("page_id","creation_date")]) )

membersX = filter(members, grepl(paste(",", 1012254620, ",", sep=""), pages) )

# now that we have p(x) & p(x&y), let's calculate p(y/x) for each page x
pred = data.frame(page_x = integer(), page_id = integer(), pxy = numeric(), py_x = numeric())

correlated = function(y) {
  as.integer( filter(membersX, grepl(paste(",", y, ",", sep=""), pages)) 
              %>% count() )
}

members_count = nrow(members)

pages_today = pages[pages$creation_date==DAY,]
pages_today = pages_today[!is.na(pages_today$page_id),]

print(system.time(
  
  for (x in pages_today$page_id) {
    print(x)
      
    px = as.integer(pages[pages$page_id==x, "n_users"])
    membersX = filter(members, grepl(paste(",", x, ",", sep=""), pages) )
    
    # Improve performance by only selecting pages viewed in common
    pages1 = serialise(membersX$pages)
    pages2 = as.list(strsplit(pages1, ",")[[1]])
    pages3 = as.integer(unique(pages2))
    pages4 = pages3[pages3 != '']
    pages4 = pages4[!is.na(pages4)]
    
    # TODO: only consider pages in the same category
    
    for (p in pages4 ) {
      pxy = correlated(p)
      if (pxy < 5 ) { next } # if pxy is too small, recommendation may not be reliable
      py = as.integer(pages[pages$page_id==p, "n_users"])
      py_x = ( pxy / px ) / ( (py - pxy) / (members_count - px) )
    
      df = data.frame(page_x=x, page_id = p, pxy = pxy, py_x = py_x)
      pred = bind_rows(pred, df)
    }
  }
))


# corr = sapply(pages$page_id, function(x) { correlated(x,"1011798701") })
# sort(corr)

# next: merge pred & pages to view page y urls to evaluate recommendations
# pred$page_id = pred$page_y
pred = merge(pred, pages[,c("page_id","url")])

# save results
saveRDS(pred, 'pred.rds')

# pred[pred$page_x==1011841822 & order(pred$py_x, decreasing = TRUE) , c("py_x","url")][1:5,]

# filter(pred, page_x == 1290569) %>%
#   select(py_x, url) %>%
#   arrange( desc(py_x) )

recommend_article <- function(page_id, pred, n=5) {
  r1 = filter(pred, page_x == page_id) %>%
    select(py_x, page_id, url) %>%
    arrange( desc(py_x) ) 
  
  r1[2:(n+1),"page_id"]
}

recommend_article(1012212411, pred)

# extract page title from url
extract_title = function(s) {
  s1 = strsplit(s,"/")[[1]]
  s2 = paste(s1[length(s1) - 1], s1[length(s1)], sep = "/")
  gsub("-2016[0-9]{4}-.*","", s2)
}

# view results
view_results = function() {
  for (x in pages_today$page_id) {
  
    r1 = filter(pred, page_x == x) %>%
      select(py_x, page_id, url) %>%
      arrange( desc(py_x) ) 
    
    r1$url = sapply(r1$url, extract_title)
    print(pages[pages$page_id==x, c("page_id","n_users","url")])
    print(r1[2:8,])
    
    cat ("Press x to exit, m for more details, any other key to see next item")
    line <- readline()
    if (line == "x") { break } 
    
    if (line == "m") {
      p1 = pred[pred$page_x == x,c("pxy","py_x","url")]
      p2 = p1[order(p1$py_x, decreasing = TRUE),]
      p2$url = sapply(p2$url, extract_title)
      print(p2[1:15,])
      
      cat ("Press x to exit, any other key to see next item")
      line <- readline()
      if (line == "x") { break } 
    }
    
  }
}
