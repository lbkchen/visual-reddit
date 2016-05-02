list.of.packages <- c("bigrquery", "ggplot2", "dplyr", "lubridate", "mosaic", "tidyr", "knitr", "beepr", "igraph", "network")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(bigrquery)
library(ggplot2)
library(mosaic)
library(knitr)
library(tidyr)
library(dplyr)
library(lubridate)

projectID = "elated-coil-129122"

sqlPosts = "SELECT subreddit, ups, downs, num_comments, title, gilded, created_utc, author, subreddit_id, id, name
FROM [Reddit.AllPosts]
WHERE YEAR(SEC_TO_TIMESTAMP(INTEGER(created_utc))) == 2014 and MONTH(SEC_TO_TIMESTAMP(INTEGER(created_utc))) == 4 and score > 10"

sqlComments = "SELECT author, ups, downs, subreddit, created_utc, name, id, parent_id, subreddit_id, gilded
FROM [Reddit.2014Comments]
WHERE YEAR(SEC_TO_TIMESTAMP(INTEGER(created_utc))) == 2014 and MONTH(SEC_TO_TIMESTAMP(INTEGER(created_utc))) == 4 and score > 30"

Posts = query_exec(query = sqlPosts, project = projectID, max_pages = Inf)
Comments = query_exec(query = sqlComments, project = projectID, max_pages = Inf)


######################################################
# NETWORK OF COMMENTS AND POSTS IN TOP 10 SUBREDDITS #
######################################################

# DECEMBER 2014
library(igraph)
library(network)
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
  select(created_utc, author, title, subreddit, subreddit_id, 
         id, name, gilded, ups, downs, num_comments) %>%
  mutate(created_utc = as.POSIXct(as.numeric(created_utc), origin="1970-01-01"),
         karma = ups - downs,
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
  select(created_utc, author, name, id, parent_id, subreddit, subreddit_id, 
         ups, downs, gilded) %>%
  mutate(created_utc = as.POSIXct(as.numeric(created_utc), origin="1970-01-01"),
         karma = ups - downs,
         hour = lubridate::hour(created_utc), 
         day_of_year = lubridate::yday(created_utc), 
         day_of_week = lubridate::wday(created_utc, label = TRUE), 
         gilded = as.factor(gilded))

# Gathering top 10 subreddits, based on total post karma in the year 2014
TopTenSubs <- Posts %>%
  group_by(subreddit) %>%
  summarize(total_karma = sum(karma), 
            subreddit_id = head(subreddit_id, 1)) %>%
  arrange(desc(total_karma)) %>%
  head(10)

SPosts <- Posts %>% filter(subreddit %in% TopTenSubs$subreddit)
SComments <- Comments %>% filter(subreddit %in% TopTenSubs$subreddit)

# Table of all nodes
NetworkPosts <-
  SPosts %>%
  select(id, subreddit, subreddit_id, title, karma, gilded) %>%
  mutate(type = "post", 
         parent_id = NA)

NetworkSubreddits <-
  SPosts %>%
  group_by(subreddit) %>%
  summarize(id = head(subreddit_id, 1)) %>%
  mutate(type = "subreddit", 
         subreddit_id = NA, 
         title = NA,
         karma = NA, 
         gilded = NA, 
         parent_id = NA)

NetworkComments <-
  SComments %>%
  select(id, parent_id, subreddit, subreddit_id, karma, gilded) %>%
  mutate(title = NA, 
         type = "comment", 
         parent_id = gsub(pattern = "^t[[:digit:]][[:punct:]]", "", parent_id))

NetworkNodes <- NetworkPosts %>% rbind(NetworkSubreddits) %>% rbind(NetworkComments)

# Table of all edges

# Subreddits to posts
NetworkEdgesA <- 
  NetworkPosts %>%
  select(from = subreddit_id, to = id, weight = karma)

# Posts to comments
NetworkEdgesB <-
  NetworkComments %>%
  select(from = parent_id, to = id, weight = karma)

NetworkEdges <- rbind(NetworkEdgesA, NetworkEdgesB) %>%
  filter(from %in% NetworkNodes$id & to %in% NetworkNodes$id)


# post = 1, subreddit = 2 "#336699", comment = 3
# Network graph
redditNetwork <- graph.data.frame(NetworkEdges, 
                                  NetworkNodes %>% mutate(subreddit = 
                                    ifelse(type == "subreddit", subreddit, "")), 
                                  directed=T) %>%
  simplify(remove.multiple = F, remove.loops = T) 
colors <- c("#9494ff", "#ff4500", "#ff5700")
V(redditNetwork)$color <- colors[ifelse(V(redditNetwork)$type == "post", 1,
                                        ifelse(V(redditNetwork)$type == "subreddit", 2, 3))] %>%
  adjustcolor(alpha.f = 0.8)
V(redditNetwork)$size <- ifelse(V(redditNetwork)$type == "subreddit", 
                                5, 
                                1 + V(redditNetwork)$karma / 1000)


plot.dir <- paste(getwd(), "/Plots.pdf", sep="")
pdf(plot.dir)
set.seed(62169869)
plot(redditNetwork, 
     layout=layout.lgl, 
     vertex.frame.color = adjustcolor("white", alpha.f = 0), 
     edge.color = adjustcolor("#ff5700", alpha.f = 0.6), 
     edge.arrow.size = 0, 
     edge.arrow.width = 0, 
     edge.lty = 1, 
     edge.width = 0.4, 
     edge.curved = 0.5, 
     vertex.label = NA, 
     vertex.label = V(redditNetwork)$subreddit, 
     vertex.label.family = "Helvetica", 
     vertex.label.font = 2, 
     vertex.label.color = "#474747", 
     vertex.label.dist = 0.3, 
     vertex.label.degree = -pi/2,
     main = "Network of subreddits, posts, and comments")

legend(x=-1.5, y=-0.6, c("Post","Subreddit", "Comment"), pch=21,
       col="#777777", pt.bg=colors, pt.cex=3, cex=.6, bty="n", ncol=1)

dev.off()  