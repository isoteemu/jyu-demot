#!/usr/bin/python2
# -*- coding: utf-8 -*-

from flask import (
    Blueprint, g, abort, flash, redirect, render_template, url_for, request, Markup
)
from flask import current_app as app
from flask_babel import _
from flask_wtf import FlaskForm
from wtforms import validators as v, ValidationError
from wtforms.fields import StringField

from google.appengine.api import taskqueue

import feedparser
from datetime import datetime, timedelta
from io import BytesIO
import re
import uuid

from . import csrf
from .utils import valid_url, url_normalize, html_parser, crawler
from .models import Feed, Asset, Subscription, Article, User, ndb, get_by_key, subscription_key
from .user import get_current_user
from .assets import asset_factory, get_entity_asset

feedparser_content_types = [
    # Content types understood by ``feedparser``
    "application/rss+xml", "application/atom+xml", "text/xml", "application/xml"
]


default_limit = 20

bp = Blueprint('feeds', __name__)


def init_app(app):
    with app.app_context():
        app.register_blueprint(bp)


r"""
FORMS
=====
"""


class AddFeedForm(FlaskForm):
    url = StringField(_(u"Add new feed"), validators=[], description=_("Discover website or Feed"))


r"""
ROUTES
======
"""

@bp.route("/test")
def page_test():
    feed_refresh()
    return ""


@bp.route("/")
def page_reader():

    # If url has param `feed`, show only that.
    feed_id = request.args.get("feed")
    show_only_feed = None
    if feed_id:
        show_only_feed = get_by_key(ndb.key.Key(Feed, feed_id))
        if not show_only_feed or not isinstance(show_only_feed, Feed):
            abort(404)

    feeds = []
    articles = []
    user = get_current_user()

    subscribed = Subscription.query(Subscription.user == user.key)

    if subscribed.count(1) == 0:
        subscribed = subscribe_into_defaults(user)

    for feed_sub in subscribed:
        feeds.append(feed_sub)

        if show_only_feed and feed_sub.feed != show_only_feed.key:
            # Skip filtered feeds.
            continue

        feed_articles = Article.query(Article.feed == feed_sub.feed).order(-Article.published)
        for article in feed_articles.fetch(default_limit):
            articles.append(article)

    # Quick hack
    def sort_datetime(a, b):
        if a.published > b.published:
            return -1
        if a.published < b.published:
            return 1
        else:
            return 0
    articles = sorted(articles, sort_datetime)

    return render_template("page-reader.html.j2", feeds=feeds, articles=articles)


@bp.route("/add", methods=('POST', 'GET'))
def page_add_feed():

    user = get_current_user()

    feed_form = AddFeedForm(data={"url": request.args.get("url")})
    feeds = {}

    discover = {}

    # TODO: Implement text searching.
    if feed_form.url.data:
        url = url_normalize(feed_form.url.data)

        # If protocol scheme missing, add it.
        if not re.match("^http[s]{0,1}://.*", url):
            url = u"http://%s" % url

        if valid_url(url):
            feeds.update(discover_feeds(url))
            if not feeds:
                flash(_(u"No feeds found"))

            if feed_form.validate_on_submit():
                # User wants to subscribe into feed.

                feed_url = url

                if feed_url not in feeds:
                    flash(_("Could not subscride into feed: Feed seems to be missing"), "error")
                    app.logger.warning(u"Tried to subscride into feed %s, but its missing from discovered feeds." % repr(feed_url))

                else:
                    # Make title safe for display purposes.
                    feed_markup = Markup(u"<em class=\"feed\">%s</em>") % feeds[feed_url].title

                    if feeds[feed_url].user_subscribed(user):
                        feed_subscride(feeds[feed_url], user).delete()
                        flash(_(u"Feed %s removed." % feed_markup))

                    else:

                        feeds[feed_url].put()
                        asset = get_entity_asset(feeds[feed_url])
                        if asset:
                            asset.put()

                        feed_subscride(feeds[feed_url], user).put()

                        if schedule_feed_update_if_needed(feeds[feed_url]):
                            flash(_(u"Feed %s added. It might take a second to show up." % feed_markup))
                        else:
                            flash(_(u"Feed %s added" % feed_markup))
        else:
            flash(_("Invalid url", "error"))

    if not feeds:
        discover = cool_feeds()

    return render_template("add_feed.html.j2", form=feed_form, feeds=feeds, discover=discover)


def subscribe_into_defaults(user):
    DEFAULTS = [
        url_for('static', filename="rss.xml", _external=True),
        u"https://www.hs.fi/rss/tuoreimmat.xml",
        u"https://www.reddit.com/r/lego/.rss",
        u"https://xkcd.com/atom.xml",
    ]

    subs = []
    for feed_url in DEFAULTS:
        try:
            k, feed = discover_feeds(feed_url).popitem()
            feed.put()

            sub = feed_subscride(feed, user)
            sub.put()

            schedule_feed_update_if_needed(feed)
            subs.append(sub)
        except Exception as e:
            app.logger.exception(e)

    flash(_(u"Defaults feeds subscrided into"))

    return subs


def cool_feeds():
    feeds = {
        _(u"Technology"): [
            _discover_feed("https://lwn.net/headlines/newrss"),
            _discover_feed("http://rss.slashdot.org/Slashdot/slashdotMain"),
            _discover_feed("https://news.ycombinator.com/rss"),
            _discover_feed("https://techcrunch.com/feed/")
        ],
        _(u"Culture"): [
            _discover_feed("https://jezebel.com/rss"),
            _discover_feed("https://www.reddit.com/r/feminism/.rss"),
            _discover_feed("https://slutever.com/atom")
        ],
        _(u"Entertainmet"): [
            _discover_feed("https://feeds.yle.fi/uutiset/v1/majorHeadlines/YLE_URHEILU.rss"),
            _discover_feed("https://www.smbc-comics.com/comic/rss"),
            _discover_feed("https://nerdist.com/feed/"),
         ]
    }
    return feeds


def _discover_feed(url):
    return discover_feeds(url).values()[0]


@bp.route("/cron/feeds-update")
def feed_refresh():
    r"""
    Create worker instances for every feed that needs updating.
    """

    o = []

    for feed in Feed.query().fetch():
        schedule_feed_update_if_needed(feed)

    return "", 202


def schedule_feed_update(feed):
    key = feed.key.urlsafe()

    task = None
    try:
        task = taskqueue.add(
            url=url_for('feeds.update_feed_task'),
            params={'feed': key},
            method='GET',
        )
    except taskqueue.TombstonedTaskError as e:
        # Error is raised usually when name task is run too frequently.
        # Log it but ignore
        app.logger.exception(e)
        pass

    return task


def schedule_feed_update_if_needed(feed):
    r"""
    Schedules feed update, if feed hasn't been updated for a while.
    """
    update = datetime.utcnow() - timedelta(minutes=feed.update_inteval)
    if not feed.updated or feed.updated < update or app.debug:
        task = schedule_feed_update(feed)
        if task:
            app.logger.info(u"Scheduling feed %s for update at %s" % (repr(feed.url), task.eta))
        return

    return False


@bp.route("/task/update-feed", methods=('POST','GET'))
@csrf.exempt
def update_feed_task():
    """
    Task for updating feed.
    """

    feed_key = request.args.get("feed")

    # Usual key checks. Notice empty returs in cases feed lookup fails.
    if not feed_key:
        app.logger.error(u"Required param feed is missing")
        return ""
    feed = ndb.key.Key(urlsafe=feed_key).get()

    if not feed or not isinstance(feed, Feed):
        app.logger.error(u"Feed %s was not found." % repr(feed_key))
        return ""

    # run actual updater.
    update_feed(feed)
    return ""


def update_feed(feed):
    # TODO: Check failure
    r = crawler().get(feed.url)
    r.raise_for_status()

    update_feed_with_rss(feed, r.content)
    update_feed_articles_with_rss(feed, r.content)

    feed.updated = datetime.utcnow()
    feed.put()


def feed_subscride(feed, user=None):
    r"""
    Subscribes user into feed. Creates new subscription object.

    :return: :class:`Subscription`
    """

    if not user:
        user = get_current_user()

    key = subscription_key(user, feed)

    entity = get_by_key(key)
    if not entity:
        entity = Subscription(key=key)

    entity.feed = feed.key
    entity.user = user.key
    entity.title = feed.title

    return entity


def discover_feeds(url, redirects=0, fallback_title=""):
    r"""
    Find feeds from website.

    :param redirects: Private property to count how many redirects
                      have occured. Limited to 3 per feed.

    :param fallback_title: Title to use, if feed doesn't specify one.
    """

    rss_feeds = {}
    url = url_normalize(url)

    # if we already know about url, don't bother looking further.
    entity = get_by_key(ndb.key.Key(Feed, url))
    if entity:
        return {entity.url: entity}

    try:
        # Fetch page, and generate suitable soup for it.
        r = crawler().get(url)
        soup = html_parser(r.text)
    except Exception as e:
        app.logger.exception(e)
        return rss_feeds

    try:
        response_type = r.headers['content-type'].split(";")[0].strip().lower()

        if response_type in feedparser_content_types:
            # If url response was suitable content type, try add it into selection
            feed_entity = feed_factory(url, type="feedparser")

            update_feed_with_rss(feed_entity, r.content)
            update_feed_articles_with_rss(feed_entity, r.content)

            if not feed_entity.title:
                feed_entity.title = fallback_title or url

            rss_feeds[feed_entity.url] = feed_entity

        elif response_type == "text/html":
            # HTML Page. Look for Feed links.
            try:
                title = soup.title.text or url
                links = soup.head.find_all("link", type=feedparser_content_types)

                # TODO: Liikaa sisennystä, jäsentele.

                for link in links:
                    # OpenGraph meta-tags do not acknowledge base-url and should always have full URLs.
                    link_url = url_normalize(link['href'])
                    if valid_url(link_url):
                        found_feeds = discover_feeds(link_url, redirects + 1, link.get('title', title))

                        logo = scrape_logo(soup)
                        if logo:
                            for f in found_feeds:
                                asset_factory(logo, found_feeds[f], weight=100)

                        rss_feeds.update(found_feeds)

            except AttributeError as e:
                app.logger.exception(e)

        else:
            app.logger.info(u"Unknown response type %s for %s" % (response_type, url))

    except Exception as e:
        app.logger.exception(e)

    return rss_feeds


def feed_factory(url, **kwargs):
    r"""
    Create new feed model

    :param url: Url for a feed.
    """

    url = url_normalize(url)

    # TODO: Use key
    feeds = Feed.query(Feed.url == url)

    if feeds.count() >= 1:
        # TODO: Should check that there is only one feed.
        feed = feeds.get()
    else:
        kwargs['url'] = url
        kwargs.setdefault("type", "feedparser")
        feed = Feed(key=ndb.Key(Feed, url))

    feed.populate(**kwargs)
    return feed


def article_factory(feed, id=None, **kwargs):
    if not id:
        content = "{title}|{published}".format(**kwargs)
        id = unicode(uuid.uuid5(namespace=uuid.NAMESPACE_OID, name=content.decode("utf-8")))

    key = ndb.key.Key(Article, id, parent=feed.key)

    article = get_by_key(key) or Article(key=key, feed=feed.key)

    article.populate(**kwargs)
    return article


def update_feed_with_rss(feed, content):
    r"""
    Parsers RSS feed content and updates :class:`Feed` entity.

    :param feed: Feed entity.
    :param content: RSS feed as text.
    """

    # Wrapped to BytesIO to prevent feedparser from doing local filepath traversal.
    feed_parser = feedparser.parse(BytesIO(content))

    # Some things provide better subtitle than title, but not always.
    # Use both if possible as middleground.
    title = feed.title
    try:
        title = feed_parser['feed'].get("title", title).decode("utf-8")
        subtitle = feed_parser['feed'].get('subtitle', "").decode("utf-8")
        if subtitle and subtitle != title:
            if subtitle.startswith(title):
                title = subtitle
            else:
                title = u"%s – %s" % (title, subtitle)

    except Exception as e:
        # There might be some corner cases this fails.
        app.logger.warning(e)

    # Strip unnecessary stuff from title.
    feed.title = Markup(title).striptags()
    if "link" in feed_parser['feed']:
        feed.link = feed_parser['feed']['link'].decode("utf-8")

    if 'image' in feed_parser['feed'] and 'href' in feed_parser['feed']['image']:
        # Create asset entity from feed image. Thease are usually poor quality.
        img = feed_parser['feed']['image']
        asset_factory(img['href'].decode("utf-8"), feed, width=img.get("width"), height=img.get("height"))


def update_feed_articles_with_rss(feed, content):
    """
    Parsers RSS feed content and update associated articles.

    :param feed: Feed entity.
    :param content: RSS feed as text.
    """

    articles = []
    feed_parser = feedparser.parse(BytesIO(content))

    for article in feed_parser['entries']:

        id = None
        title = ""
        content = ""
        url = ""
        published = None
        try:
            # Find textual stuff
            id = article.get("id", None)
            title = Markup(article["title"]).striptags()
            url = article["link"]

            updated = article.get("updated_parsed") or article.get("published_parsed")
            published = datetime(*updated[:6])

            if "content" in article:
                if type(article['content']) == list:
                    content = u"%s" % article['content'][0].get("value")
                else:
                    content = u"%s" % article['content'].get("value")
            else:
                content = u"%s" % article.get("summary", "")

            article_entity = article_factory(feed, id=id, title=title, url=url,
                                             published=published, content=content)

            articles.append(article_entity)

            # Try finding image.
            soup = html_parser(content)
            img = soup.find("img")
            if img:
                asset_factory(img['src'], article_entity, weight=10)

            for media in article.get("media_content", []):
                if "url" not in media:
                    continue
                if not valid_url(media["url"]):
                    continue

                if "type" in media and not media['type'].startswith("image/"):
                    app.logger.debug(u"Found media content, but its not declared as image: %s" % repr(media['url']))
                    continue

                asset_factory(media['url'], article_entity, weight=50)

            # TODO: Enclosure myös

        except Exception as e:
            app.logger.exception(e)

    return articles


def filter_content(html):
    return Markup(html).striptags()


def scrape_logo(soup):
    """
    Look for logo from webpage.
    """

    # Try lookin suitable meta tag, and return most suitable one.
    # TODO: enumerate candidates.
    logo_uri = None
    try:
        meta = soup.head.find("meta", property="og:image") or soup.head.find("meta", {"name": "twitter:image"})
        logo_uri = meta['content']
        if logo_uri:
            app.logger.info("Found logo from meta property %s", meta)
            return logo_uri
    except Exception as e:
        app.logger.exception(e)
        pass

    try:
        meta = soup.head.find("link", rel="apple-touch-icon-precomposed")
        logo_uri = meta['href']
        if logo_uri:
            app.logger.info("Found logo from link property %s", meta)
            return logo_uri
    except Exception as e:
        app.logger.exception(e)
        pass

    return None
