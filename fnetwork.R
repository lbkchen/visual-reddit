######################################################
# NETWORK OF COMMENTS AND POSTS IN TOP 10 SUBREDDITS #
######################################################

library(DataComputing)
library(igraph)
library(network)

filepath <- "/Users/kenchen/MDST/visual_reddit/"
Posts <- read.file(paste(filepath, "PostSample_10k.csv", sep=""))
Comments <- read.file(paste(filepath, "CommentSample_10k.csv", sep=""))

# Cleaning up Posts

# Post variables
# [1] "domain"                 "subreddit"              "selftext"               "saved"                 
# [5] "id"                     "from_kind"              "gilded"                 "from"                  
# [9] "stickied"               "title"                  "num_comments"           "score"                 
# [13] "retrieved_on"           "over_18"                "thumbnail"              "subreddit_id"          
# [17] "hide_score"             "link_flair_css_class"   "author_flair_css_class" "downs"                 
# [21] "archived"               "is_self"                "from_id"                "permalink"             
# [25] "name"                   "created"                "url"                    "author_flair_text"     
# [29] "quarantine"             "author"                 "created_utc"            "link_flair_text"       
# [33] "ups"                    "distinguished"    

Posts <-
  Posts %>%
  mutate(retrieved_on = as.POSIXct(retrieved_on, origin="1970-01-01"), 
         created_utc = as.POSIXct(created_utc, origin="1970-01-01"), 
         created = as.POSIXct(created, origin="1970-01-01")) %>%
  select(created_utc, author, title, subreddit, subreddit_id, 
         id, name, gilded, ups, downs, num_comments, retrieved_on, over_18) %>%
  mutate(karma = ups - downs, 
         hour = lubridate::hour(created_utc), 
         day_of_year = lubridate::yday(created_utc), 
         day_of_week = lubridate::wday(created_utc, label = TRUE), 
         morning = hour < 12,
         working_hours = hour > 8 & hour < 18, 
         gilded = as.factor(gilded))



# Cleaning up comments

# Comment variables
# [1] "body"                   "score_hidden"           "archived"               "name"                  
# [5] "author"                 "author_flair_text"      "downs"                  "created_utc"           
# [9] "subreddit_id"           "link_id"                "parent_id"              "score"                 
# [13] "retrieved_on"           "controversiality"       "gilded"                 "id"                    
# [17] "subreddit"              "ups"                    "distinguished"          "author_flair_css_class"
# [21] "removal_reason"        

Comments <-
  Comments %>%
  mutate(created_utc = as.POSIXct(created_utc, origin="1970-01-01"),
         retrieved_on = as.POSIXct(retrieved_on, origin="1970-01-01")) %>%
  select(created_utc, author, body, name, id, parent_id, subreddit, subreddit_id, 
         ups, downs, gilded) %>%
  mutate(karma = ups - downs,
         hour = lubridate::hour(created_utc), 
         day_of_year = lubridate::yday(created_utc), 
         day_of_week = lubridate::wday(created_utc, label = TRUE), 
         gilded = as.factor(gilded))

# Gathering top 10 subreddits, based on total post karma in the year 2014
TopTenSubs <- Posts %>%
  # filter(year(created_utc) == 2014 & subreddit != "") %>% # uncomment to filter year 2014
  group_by(subreddit) %>%
  summarize(total_karma = sum(karma), 
            subreddit_id = head(subreddit_id, 1)) %>%
  arrange(desc(total_karma)) %>%
  head(10)

# Table of all nodes

Nodes <- 