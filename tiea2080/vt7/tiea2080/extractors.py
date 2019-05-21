# -*- coding: utf-8 -*-

r"""
Functions to lookup Assets from content. Some external services,
like imgur, is translated from link into direct asset url in
:func:`lookup()`.
"""

from __future__ import unicode_literals

from flask import current_app

from urlparse import urlparse
from functools import wraps
import re

from .utils import (
    html_parser,
    crawler,
    valid_url,
    url_normalize
)

url_extractors = {
}


def url_extractor(domains):
    if isinstance(domains, basestring):
        domains = [domains]

    def decorator_register(f):
        for domain in domains:
            domain = domain.lower()
            url_extractors.setdefault(domain, [])
            url_extractors[domain].append(f)

        return f

    return decorator_register


def lookup(url):
    # Split domain into parts, and go from major to minor.

    if not valid_url(url):
        raise ValueError("Url %s is not valid" % repr(url))

    resources = set()

    url = url_normalize(url)

    parts = urlparse(url)
    domain = parts.netloc
    domain_parts = domain.split(".")

    # Lookup extraction functions from `www.example.com` -> `example.com` -> `com`.
    for i in range(0, len(domain_parts)):
        lookup_key = ".".join(domain_parts[-len(domain_parts) + i:])
        if lookup_key in url_extractors:
            for f in url_extractors[lookup_key]:
                resources.add(f(url))

    return list(filter(None, resources))


@url_extractor(["i.redd.it"])
def reddit_extractor(url):
    return url.split("?")[0]


@url_extractor("imgur.com")
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
        # Album, make request to retrieve album contents.
        api_url = "https://api.imgur.com/3/album/%s" % album_match.group(1)
        api_request = crawler().get(api_url, headers={
            "Authorization": "Client-ID %s" % client_id
        })
        data = api_request.json()["data"]
        cover_id = data.get("cover")

        # Iterate all album images, and look for cover image.
        for image in data.get("images", []):
            link = image['link']
            if not cover_id or cover_id == image['id']:
                if not image.get("type", "image/").startswith("image/"):
                    # If its not image, change extension to png
                    splitted = link.split(".")
                    splitted[-1] = "png"
                    link = ".".join(splitted)

                return link



@url_extractor("gfycat.com")
def gfycat_extractor(url):
    # Try matching ID from url
    gfycat_item = re.search(r"/(?:(?:ifr|gifs\/detail)\/)?(\w+)(?:\.gif)?$", url, re.IGNORECASE + re.UNICODE)
    if gfycat_item:
        # Return item info if such is found.
        r = crawler().get("https://api.gfycat.com/v1/gfycats/%s" % gfycat_item.group(1))
        data = r.json()
        return data.get("gfyItem", {}).get("posterUrl")


@url_extractor("streamable.com")
def streamable_extractor(url):
    # Streamable provides api, but to make things easier,
    # let's skip it. https://api.streamable.com/videos/{code}

    code_re = r"/(?:[es]/)?(\w+)(?:/\w+)?$"
    match = re.search(code_re, url, re.UNICODE)
    if match:
        return u"https://thumbs-east.streamable.com/image/%s.jpg" % match.group(1)


@url_extractor("streamja.com")
def streamja_extractor(url):
    match = re.search(r"^https?://streamja.com/([^/]+)", url, re.UNICODE)
    if match:
        code = match.group(1)
        short = code[0:2].lower()
        return "https://upload.streamja.com/i/%s/%s.jpg" % (short, code)


@url_extractor(["instagram.com", "instagr.am"])
def instagram_extractor(url):
    code_re = r"/p/([a-zA-Z0-9_-]{10,})(?:/|$)"
    match = re.search(code_re, url, re.UNICODE)
    if match:
        return u"https://instagram.com/p/%s/media/" % match.group(1)


def extract_assets(content):
    r"""
    Extract assets from HTML content.

    :returns: Matrix of asset urls. First column is asset url, and second
              is asset weight as defined in `asset_extractors`.
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
            matched_assets = callback(match, content) or []
            if isinstance(matched_assets, basestring):
                matched_assets = [matched_assets]

            # Add weight info into list
            assets += [(asset, weight) for asset in matched_assets]

    return list(set(assets))


def _lookup_from_href(soup, content):
    resources = []
    url = soup['href']
    if valid_url(url):
        resources += lookup(url)
    return resources


def _lookup_from_img(soup, content):
    resources = []
    url = soup['src']
    if valid_url(url):
        resources += lookup(url)
    return resources


def _direct_lookup(soup, content):
    url = soup.get("href") or soup.get("href")
    if valid_url(url):
        return [url]
    return []


# First entry defines css selector, second is extractor to be used,
# and third is optional weight.
asset_extractors = [
    # Common tags
    ('a[href^="http"]', _lookup_from_href, 100),
    ('img[src^="http"]', _lookup_from_img, 90),

    # Direct hotlinks
    ('a[href$=".jpg"]', _direct_lookup, 40),
    ('a[href$=".jpeg"]', _direct_lookup, 40),
    ('a[href$=".png"]', _direct_lookup, 40),
    ('a[href$=".gif"]', _direct_lookup, 40),

    # Images
    ('img[src$=".jpg"]', _direct_lookup, 50),
    ('img[src$=".jpeg"]', _direct_lookup, 50),
    ('img[src$=".png"]', _direct_lookup, 50),
    ('img[src$=".gif"]', _direct_lookup, 50),

]
