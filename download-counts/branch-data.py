#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
    branch-data.py: Get branch data for  VIVO and Vitro repositories at GitHub.  Write  TSV
    data for use by humans trying to clean-up the VIVO and Vitro branches

"""

from github import Github

g = Github("username", "password")  # Enter Github credentials here
repos = g.get_user().get_repos()
for repo in repos:
    if repo.url == 'https://api.github.com/repos/vivo-project/Vitro':
        for b in repo.get_branches():
            branch = repo.get_branch(b.name)
            commit = repo.get_commit(sha=branch.commit.sha)
            try:
                author = commit.author.login
            except AttributeError:
                author = "unknown"
            print(branch.name, author, commit.last_modified, sep="\t")





