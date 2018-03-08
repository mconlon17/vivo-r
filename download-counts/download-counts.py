#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
    download-counts.py: Count downloads from VIVO repositories at GitHub.  Write  a file
    for use in the R programs for VIVO tracking data

"""
# TODO Use the API to get a list of repositories, iterate over them, producing counts for each
import requests
import json

__author__ = "Michael Conlon"
__copyright__ = "Copyright (c) 2018 Michael Conlon"
__license__ = "Apache-2"
__version__ = "0.0.0"

releases = requests.get('https://api.github.com/repos/vivo-project/VIVO/releases')
total_count = 0
for release in releases.json():
#    print json.dumps(release, indent=4)
    release_count = 0
    for x in release["assets"]:
        if "download_count" in x:
            print "\t", "Asset name", "\t", x["name"], "\t", x["download_count"]
            release_count += x["download_count"]
    print release["name"], "\t", "Count over assets", release_count, "\n"
    total_count += release_count
print "Super duper total count", total_count
