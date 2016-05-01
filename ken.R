library(DataComputing)
library(XML)

filepath <- "/Users/kenchen/MDST/visual_reddit/"
Posts <- read.file(paste(filepath, "Posts.csv", sep=""))
Comments <- read.file(paste(filepath, "2014Comments.csv", sep=""))

Posts %>% variable.names()

# [1] "domain"                 "subreddit"              "selftext"               "saved"                 
# [5] "id"                     "from_kind"              "gilded"                 "from"                  
# [9] "stickied"               "title"                  "num_comments"           "score"                 
# [13] "retrieved_on"           "over_18"                "thumbnail"              "subreddit_id"          
# [17] "hide_score"             "link_flair_css_class"   "author_flair_css_class" "downs"                 
# [21] "archived"               "is_self"                "from_id"                "permalink"             
# [25] "name"                   "created"                "url"                    "author_flair_text"     
# [29] "quarantine"             "author"                 "created_utc"            "link_flair_text"       
# [33] "ups"                    "distinguished"    

# Cleaning up Posts
Posts <-
  Posts %>%
  mutate(retrieved_on = as.POSIXct(retrieved_on, origin="1970-01-01"), 
         created_utc = as.POSIXct(created_utc, origin="1970-01-01"), 
         created = as.POSIXct(created, origin="1970-01-01")) 
# Top karma posts based on time
