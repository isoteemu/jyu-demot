# -*- coding: utf-8 -*-

from __future__ import unicode_literals

from flask import current_app

from .utils import (
    html_parser,
    crawler,
    urlencode,
    valid_url,
    url_normalize
)

from functools import wraps

import re


def url_extractor(f, domain=None):

    @wraps(f)
    def wrapper(*args, **kwds):
        url = args[0].get('href') or args[0].get('src')

        if valid_url(url):
            url = url_normalize(url)
            return f(url)

        else:
            current_app.logger.debug("Not valid url %s for extractor %s", url, f.__name__)
    return wrapper


@url_extractor
def link_extractor(url):
    return url


@url_extractor
def imgurl_extractor(url):
    url = url.split("?")[0]

    client_id = current_app.config.get("IMGUR_API_CLIENT_ID")

    hotlink_re = r"^https?://(?:i.)?imgur\.com/(\w+)(?:\.\w+)(?:[/#]|$)"
    album_re = r"^https?://(?:m\.|www\.)?imgur\.com/a/(\w+)(?:[/#]|$)"
    hotlink_match = re.search(hotlink_re, url)

    album_match = re.search(album_re, url)
    if hotlink_match:
        url = "https://i.imgur.com/%s.png" % hotlink_match.group(1)
        return url
    elif album_match:
        api_url = "https://api.imgur.com/3/album/%s" % album_match.group(1)
        api_request = crawler().get(api_url, headers={
            "Authorization": "Client-ID %s" % client_id
        })
        data = api_request.json()["data"]
        cover_id = data.get("cover")
        for image in data.get("images", []):
            if not cover_id or cover_id == image['id']:
                return image['link']

    # webpage = re.search("^https?://imgur.com/([a-z0-9/]+)$", url, re.IGNORECASE + re.UNICODE)
    # if webpage:
    #     return "https://i.imgur.com/%s.png" % webpage.group(1)
    # TODO: Handle albums


@url_extractor
def gfycat_extractor(url):
    # Try matching ID from url
    gfycat_item = re.search(r"/(?:(?:ifr|gifs\/detail)\/)?(\w+)(?:\.gif)?$", url, re.IGNORECASE + re.UNICODE)
    if gfycat_item:
        # Return item info if such is found.
        r = crawler().get("https://api.gfycat.com/v1/gfycats/%s" % gfycat_item.group(1))
        data = r.json()
        return data.get("gfyItem", {}).get("posterUrl")


@url_extractor
def streamja_extractor(url):
    match = re.search(r"^https?://streamja.com/([^/]+)", url, re.IGNORECASE + re.UNICODE)
    if match:
        code = match.group(1)
        short = code[0:2].lower()
        return "https://upload.streamja.com/i/%s/%s.jpg" % (short, code)


def extract_assets(content):
    r"""
    Extract assets from content
    """
    soup = html_parser(content)

    assets = []

    for _extractor in asset_extractors:
        if len(_extractor) == 2:
            selector, callback = _extractor
            weight = 50
        elif len(_extractor) == 3:
            selector, callback, weight = _extractor
        else:
            raise IndexError("Defined extractor needs to have 2 or 3 values; selector, callback, and weight")
        matches = soup.select(selector) or []

        for match in matches:
            url = callback(match, content)
            if url:
                assets.append((url, weight))

    return assets


# First entry defines css selector, second is extractor to be used,
# and third is optional weight.
asset_extractors = [

    # Link to reddit image service.
    ('a[href*="//i.redd.it/"]', link_extractor, 100),
    ('a[href*="//i.imgur.com/"]', imgurl_extractor, 100),
    ('a[href*="//imgur.com"]', imgurl_extractor, 100),
    ('a[href*="//gfycat.com/"]', gfycat_extractor, 100),
    ('a[href*="//streamja.com/"]', streamja_extractor, 100),

    # Common image tags
    ('img[src^="http"]', link_extractor, 40),
    ('a[href$=".jpeg"]', link_extractor, 50),
    ('a[href$=".png"]', link_extractor, 50),
    ('a[href$=".gif"]', link_extractor, 50),
]
