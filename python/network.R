library(DataComputing)
library(igraph)
library(network)

wd <- "/Users/kenchen/MDST/visual_reddit/python/"
Posts <- read.file(paste0(wd, "pics_posts.csv"))
Comments <- read.file(paste0(wd, "pics_comments.csv"))

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

SubredditEdges <- data.frame(from = c(), to = c(), weight = c(), created_utc = c())

for (i in 1:nrow(NetworkSubreddits)) {
  for (j in i:nrow(NetworkSubreddits)) {
    if (i != j) {
      SubredditEdges <- SubredditEdges %>% rbind(
        data.frame(from = c(NetworkSubreddits$id[i]), 
                   to = c(NetworkSubreddits$id[j]), 
                   weight = c(NetworkSubreddits$karma[i]), created_utc = NA)
      )
    }
  }
}

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

colors <- c("#557BED", "#F2E11F", "#FA6C23")
V(redditNetwork)$color <- colors[V(redditNetwork)$type] %>% adjustcolor(alpha.f = 0.66)

max_subreddit_karma <- max((V(redditNetwork)[type==1])$karma)
max_post_karma <- max((V(redditNetwork)[type==2])$karma)
max_comment_karma <- max((V(redditNetwork)[type==3])$karma)

V(redditNetwork)$size <- ifelse(V(redditNetwork)$type == 1, 
                                6 + 9 * V(redditNetwork)$karma / max_subreddit_karma, # subreddit
                                ifelse(V(redditNetwork)$type == 2, 
                                       3 + 7 * V(redditNetwork)$karma / max_post_karma, # post
                                       0.6 + 5 * V(redditNetwork)$karma / max_comment_karma)) # comment

max_weight <- max(E(redditNetwork)$weight)
E(redditNetwork)$width <- 0.7 + 7 * E(redditNetwork)$weight / max_weight

set.seed(42814093)
plot(redditNetwork, 
     layout=layout.lgl, 
     vertex.frame.color = NA, 
     edge.color = adjustcolor("#FFC3A3", alpha.f = 0.66), 
     edge.arrow.size = 0, 
     edge.arrow.width = 0, 
     edge.lty = 1, 
     # edge.width = 0.6, 
     edge.curved = 0.1, 
     vertex.label = V(redditNetwork)$subreddit, 
     vertex.label.family = "Helvetica", 
     vertex.label.font = 2, 
     vertex.label.cex= 0.8, 
     vertex.label.color = "#474747", 
     vertex.label.dist = 0.2, 
     vertex.label.degree = -pi/2,
     main = "Network of subreddits, posts, and comments")

legend(x=-1.1, y=-0.4, c("Subreddit", "Post", "Comment"), pch=21,
       col="#ffffff", pt.bg=colors, pt.cex=3, cex=.9, bty="n", ncol=1)