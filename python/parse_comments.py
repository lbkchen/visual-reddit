__author__ = 'kenchen'

import praw
import csv


def combine_subreddits(subs):
    return "+".join(subs)


r = praw.Reddit("Network visualization data scraper v1.0 by u/k-a-n")

subreddits = ["funny"]

# Getting top [limit] posts from multi-reddit defined from subreddits above
top_subreddits = r.get_subreddit(combine_subreddits(subreddits))
top_posts = top_subreddits.get_top_from_all(limit=10)

# Write posts csv table
with open('pics_posts.csv', 'wb') as csv_posts:
    post_attrs = ["id", "subreddit_id", "subreddit", "author", "title", "created_utc", "ups", "downs", "gilded"]
    pwriter = csv.writer(csv_posts, delimiter=',')
    pwriter.writerow(post_attrs)

    # Write comments csv table
    with open('pics_comments.csv', 'wb') as csv_comments:
        comment_attrs = ["id", "parent_id", "subreddit", "author", "created_utc", "ups", "downs", "gilded"]
        cwriter = csv.writer(csv_comments, delimiter=',')
        cwriter.writerow(comment_attrs)

        for post in top_posts:
            # Write this post with variables post_attrs to file
            pwriter.writerow([getattr(post, attr) for attr in post_attrs])

            # Replace MoreComments objects with more Comments (expand comment tree deeper)
            post.replace_more_comments(limit=post.num_comments // 100)
            post_comments = praw.helpers.flatten_tree(post.comments)

            # Write all Comment objects in the flattened comment tree
            for comment in post_comments:
                if type(comment) != praw.objects.MoreComments:
                    cwriter.writerow([getattr(comment, attr) for attr in comment_attrs])
