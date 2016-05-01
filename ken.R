library(DataComputing)
library(XML)

# Some general stuff about karma thresholds:
#   - controversial: < 0 karma
#   - standard: >= 0 karma
#   - good: >= 100 karma
#   - viral: >= 500 karma
#   - front page: >= 3000 karma

filepath <- "/Users/kenchen/MDST/visual_reddit/"
Posts <- read.file(paste(filepath, "PostSample_10k.csv", sep=""))
Comments <- read.file(paste(filepath, "CommentSample_10k.csv", sep=""))

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
         created = as.POSIXct(created, origin="1970-01-01")) %>%
  select(created_utc, author, title, subreddit, subreddit_id, 
         id, name, gilded, ups, downs, num_comments, retrieved_on, over_18) %>%
  mutate(karma = ups - downs, 
         hour = lubridate::hour(created_utc), 
         day = lubridate::day(created_utc), 
         gilded = as.factor(gilded))

# Average karma of posts based on time of day
KarmaByHour <- 
  Posts %>%
  group_by(hour) %>%
  summarize(karma = mean(karma))

KarmaByHour %>% 
  ggplot(aes(x = hour, y = karma, fill = karma)) + geom_bar(stat = "identity") +
  ggtitle("Average karma of posts by hour of day")

# Average karma for viral (>= 500 karma) posts based on time of day
ViralKarmaByHour <- 
  Posts %>%
  filter(karma >= 500) %>%
  group_by(hour) %>%
  summarize(karma = mean(karma))

ViralKarmaByHour %>% 
  ggplot(aes(x = hour, y = karma, fill = karma)) + geom_bar(stat = "identity") +
  ggtitle("Average viral karma of posts by hour of day")

# Length of post title
PostTitles <-
  Posts %>%
  mutate(len_title = nchar(title))

PostTitles %>%
  ggplot(aes(x = len_title, y = karma)) + geom_point(aes(col = gilded)) + 
  ggtitle("Karma earned vs. length of post title")

# Length of post title (exc. any posts <100 karma to reduce noise)
PostTitlesTenPlus <-
  PostTitles %>%
  filter(karma >= 100)

PostTitlesTenPlus %>%
  ggplot(aes(x = len_title, y = karma)) + geom_point(aes(col = gilded)) + 
  ggtitle("Karma earned vs. length of post title (for 100+ karma posts)")

# Which subreddits give the most karma?

## One metric: posts usually become 'viral' or hit the front page of reddit once they've
## reached 3000 karma. We will take all posts over 3000 karma, and scale their karma relatively
## to 3000 (e.g. 6000 becomes 6000/3000 = 2 points in our new metric), and figure out which 
## subreddits (subforums) have the most overall points

karmaThreshold <- 3000
ViralSubreddits <-
  Posts %>%
  filter(karma >= karmaThreshold) %>%
  mutate(points = karma / karmaThreshold) %>%
  group_by(subreddit) %>%
  summarize(total_points = sum(points)) %>%
  arrange(desc(total_points)) %>%
  head(25)

ViralSubreddits %>%
  ggplot(aes(x = subreddit, y = total_points, fill = total_points)) + geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 12)) + 
  ggtitle("Subreddits with the most viral posts")

## Another metric: simply the average karma per post in the top 25 subreddits (current data
## is not representative because of small sample size.)

AvgKarmaSubreddits <-
  Posts %>%
  group_by(subreddit) %>%
  summarize(average_karma = mean(karma)) %>%
  arrange(desc(average_karma)) %>%
  head(25)

AvgKarmaSubreddits %>%
  ggplot(aes(x = subreddit, y = average_karma, fill = average_karma)) + geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 12)) + 
  ggtitle("Average karma of posts in the top 25 subreddits")
