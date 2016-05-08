library(DataComputing)
library(igraph)
library(network)

wd <- "/Users/kenchen/MDST/visual_reddit/python/"
Posts <- read.file(paste0(wd, "postssmall.csv"))
Comments <- read.file(paste0(wd, "commentssmall.csv"))

# Cleaning up Posts and Comments

Posts <- Posts %>%
  mutate(created_utc = as.POSIXct(as.numeric(created_utc), origin="1970-01-01"), 
         hour = lubridate::hour(created_utc), 
         karma = ups - downs, 
         gilded = as.factor(gilded))

Comments <- Comments %>%
  mutate(created_utc = as.POSIXct(as.numeric(created_utc), origin="1970-01-01"), 
         hour = lubridate::hour(created_utc), 
         karma = ups - downs, 
         gilded = as.factor(gilded))

# Creating data frame of network nodes
# 1 - subreddit
# 2 - post
# 3 - comment

NetworkSubreddits <- Posts %>%
  select(-c(author, title, ups, downs, gilded)) %>%
  group_by(subreddit) %>%
  summarize(id = head(subreddit_id, 1), 
            karma = sum(karma)) %>%
  mutate(type = 1, 
         subreddit_id = NA, 
         parent_id = NA, 
         created_utc = NA, 
         hour = NA)

NetworkPosts <- Posts %>%
  select(-c(author, title, ups, downs, gilded)) %>%
  mutate(type = 2, 
         id = paste0("t3_", id), 
         parent_id = subreddit_id, 
         subreddit_id = NA)

NetworkComments <- Comments %>%
  select(-c(author, ups, downs, gilded)) %>%
  mutate(type = 3, 
         id = paste0("t1_", id), 
         subreddit_id = NA)

NetworkNodes <- NetworkSubreddits %>% rbind(NetworkPosts) %>% rbind(NetworkComments) %>% 
  select(id, subreddit, karma, type, subreddit_id, parent_id, created_utc, hour)

# Creating table of network adjacencies

SubredditEdges <- data.frame(from = c("t5_2qh33"), to = c("t5_2qh0u"), weight = c(50000), created_utc = NA)

NetworkEdges <- NetworkNodes %>%
  filter(type != 1) %>%
  select(from = parent_id, to = id, weight = karma, created_utc) %>%
  rbind(SubredditEdges)

# Creating network

redditNetwork <- graph.data.frame(NetworkEdges, 
                                  NetworkNodes %>% 
                                    mutate(subreddit = ifelse(type == 1, subreddit, "")), 
                                  directed = T) %>%
  simplify(remove.multiple = F, remove.loops = T)

colors <- c("#ff4500", "#9494ff", "#ff5700")
V(redditNetwork)$color <- colors[V(redditNetwork)$type] %>% adjustcolor(alpha.f = 0.8)

V(redditNetwork)$size <- ifelse(V(redditNetwork)$type == 1, 
                                # 10, 
                                5 + V(redditNetwork)$karma / 30000, 
                                1 + V(redditNetwork)$karma / 5000)

plot(redditNetwork, 
     layout=layout.lgl, 
     vertex.frame.color = adjustcolor("white"), 
     edge.color = adjustcolor("#ff5700", alpha.f = 0.6), 
     edge.arrow.size = 0, 
     edge.arrow.width = 0, 
     edge.lty = 1, 
     edge.width = 0.6, 
     edge.curved = 0.1, 
     vertex.label = V(redditNetwork)$subreddit, 
     vertex.label.family = "Helvetica", 
     vertex.label.font = 2, 
     vertex.label.color = "#474747", 
     vertex.label.dist = 0.3, 
     vertex.label.degree = -pi/2,
     main = "Network of subreddits, posts, and comments")

legend(x=-1.5, y=-0.6, c("Subreddit", "Post", "Comment"), pch=21,
       col="#ffffff", pt.bg=colors, pt.cex=3, cex=.6, bty="n", ncol=1)