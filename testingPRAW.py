import praw
import pprint

user_agent = "User-Agent: Karma Breakdown: v1.0 by /u/kurumia"
r = praw.Reddit(user_agent = user_agent)

user_name = "Spiritor_XYZ"
user = r.get_redditor(user_name)

thing_limit = 10
gen = user.get_submitted(limit = thing_limit)

karma_by_subreddit = {}
for thing in gen:
    subreddit = thing.subreddit.display_name
    karma_by_subreddit[subreddit] = karma_by_subreddit.get(subreddit, 0) + thing.score

print(karma_by_subreddit)
pprint.pprint(karma_by_subreddit)
