__author__ = 'kenchen'

import praw
import csv

# FUNCTIONS


def combine_subreddits(subs):
    return "+".join(subs)


r = praw.Reddit("Comment parser v1.0 by u/Kurumia")

subreddits = ["funny", "AdviceAnimals", "pics", "aww", "WTF",
              "todayilearned", "gaming", "videos", "gifs", "leagueoflegends"]

top_subreddits = r.get_subreddit(combine_subreddits(subreddits))
top_posts = top_subreddits.get_top_from_all(limit=100)

with open('posts.csv', 'wb') as csv_posts:
    post_attrs = ["id", "subreddit_id", "subreddit", "title", "created_utc", "ups", "downs", "gilded"]
    pwriter = csv.writer(csv_posts, delimiter=',')
    pwriter.writerow(post_attrs)

    with open('comments.csv', 'wb') as csv_comments:
        comment_attrs = ["id", "parent_id", "subreddit", "created_utc", "ups", "downs", "gilded"]
        cwriter = csv.writer(csv_comments, delimiter=',')
        cwriter.writerow(comment_attrs)

        for post in top_posts:
            pwriter.writerow([getattr(post, attr) for attr in post_attrs])
            post.replace_more_comments(limit=post.num_comments // 200)
            post_comments = praw.helpers.flatten_tree(post.comments)
            for comment in post_comments:
                if type(comment) != praw.objects.MoreComments:
                    cwriter.writerow([getattr(comment, attr) for attr in comment_attrs])
