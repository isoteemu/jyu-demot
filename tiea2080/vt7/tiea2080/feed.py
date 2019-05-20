# -*- coding: utf-8 -*-

from flask import (
    Blueprint,
    g,
    flash,
    render_template,
    url_for,
    Markup,
    redirect,
    request,
    abort,
    jsonify,
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

from . import csrf, cache, memcache
from .utils import valid_url, url_normalize, html_parser, crawler
from .models import (
    Feed,
    Asset,
    Subscription,
    Article,
    User,
    Saved,
    ndb,
    get_by_key,
    subscription_key,
)
from .user import get_current_user
from .subsctiptions import (
    user_subscriptions,
    unsubscribe,
    memcache_key_subscriptions,
)
from .assets import asset_factory, get_entity_asset
from .extractors import extract_assets

feedparser_content_types = [
    # Content types understood by ``feedparser``
    "application/rss+xml", "application/atom+xml", "text/xml", "application/xml"
]

default_limit = 24

memcache_key_latest = "feeds.latest:%s"

bp = Blueprint('feeds', __name__)


def init_app(app):
    app.register_blueprint(bp)


r"""
FORMS
=====
"""


class AddFeedForm(FlaskForm):
    url = StringField(_(u"Add new feed"), validators=[], description=_("Discover website or Feed"))


@bp.route("/")
def page_reader():
    r"""
    Default reader page.
    """

    next_page = None
    prev_page = None
    requested_cursor = request.args.get('cursor')

    feeds = []
    show_feed_keys = []
    articles = []

    # User subscribed feeds. Even if `feed_id` is defined,
    # collect subscribed feeds and pass them to template.
    user = get_current_user()
    subscribed_feeds = user_subscriptions(user)

    # Fetch feeds at background, while we collect articles.
    sub_feed_keys = [sub.feed for sub in subscribed_feeds]
    feed_async_future = ndb.get_multi_async(sub_feed_keys)

    # If url has param `feed`, show only that.
    feed_id = request.args.get("feed")
    show_only_feed = None
    if feed_id:
        show_only_feed = get_by_key(ndb.key.Key(Feed, feed_id))
        if not show_only_feed or not isinstance(show_only_feed, Feed):
            # If feed doesn't exists, fail with 404.
            app.logger.debug("Requested feed doesnt' exists: %s", feed_id)
            abort(404)
        else:
            # Add it into query.
            show_feed_keys.append(show_only_feed.key)
    else:
        # If not specified feed was requested, fetch all subscribed feeds.
        for feed_sub in subscribed_feeds:
            show_feed_keys.append(feed_sub.feed)

    # Query and cursor building
    # =========================

    cursor = ndb.Cursor(urlsafe=requested_cursor)

    # Build queries for current and previous page.
    q = Article.query(
        Article.feed.IN(show_feed_keys)
    )
    q_next = q.order(-Article.published, Article._key)

    # Fetch items from current cursor.
    articles, next_cursor, has_more = q_next.fetch_page(default_limit, start_cursor=cursor)

    if has_more and next_cursor:
        # Link for next cursor.
        next_page = url_for(".page_reader", cursor=next_cursor.urlsafe(), feed=feed_id)

    if requested_cursor:
        # Look for previous page.
        prev_cursor = memcache.get("feed.reader.prev_cursor:%s" % next_cursor.urlsafe())
        if prev_cursor is None:
            # If "previous" cursor not in memstore, make query for reverse cursor
            q_prev = q.order(Article.published, Article._key)
            prev_articles, prev_cursor, had_more = q_prev.fetch_page(default_limit, start_cursor=cursor)
            # Store cursor into memstore
            memcache.set("feed.reader.prev_cursor:%s" % next_cursor.urlsafe(), prev_cursor or "")

        if prev_cursor:
            prev_page = url_for(".page_reader", cursor=prev_cursor.urlsafe(), feed=feed_id)

    # Collect feeds from async query.
    for feed_future in feed_async_future:
        feeds.append(feed_future.get_result())

    return render_template("page-reader.html.j2", feeds=feeds, articles=articles, next_page=next_page, prev_page=prev_page)


@bp.route("/save/<article_id>", methods=('POST', 'DELETE', 'GET'))
def article_save(article_id):
    r"""
    Web callback for saving articles.
    """

    status = {"status": "OK"}

    user = get_current_user()
    if not user.is_authenticated:
        abort(403)

    # Load associated article
    key = ndb.key.Key(urlsafe=article_id)
    if key.kind() != "Article":
        abort(404)

    article = get_by_key(key)
    if not article:
        abort(404)

    save = saved_factory(article, user)

    try:
        if request.method == "POST":
            save.put()
            status['status'] = "OK"
        elif request.method == "DELETE":
            save.delete()
            status['status'] = "OK"
    except Exception as e:
        app.logger.exception(e)
        status['status'] = "ERROR"
        status['message'] = unicode(e)

    return jsonify(status)


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
                    flash(_("Could not subscride into feed."), "error")
                    app.logger.warning(u"Tried to subscride into feed %s, but its missing from discovered feeds." % repr(feed_url))

                else:
                    # Make title safe for display purposes.
                    feed_markup = Markup("<em class=\"feed\">%s</em>") % feeds[feed_url].title

                    subscription = user.has_subscribed(feeds[feed_url])
                    if subscription:
                        unsubscribe(subscription)
                        flash(_(u"Feed %s removed." % feed_markup))

                    else:
                        feeds[feed_url].put()
                        asset = get_entity_asset(feeds[feed_url])
                        if asset:
                            asset.put()

                        subscription = feed_subscride(feeds[feed_url], user)
                        subscription.put()

                        if schedule_feed_update_if_needed(feeds[feed_url]):
                            flash(_(u"Feed %s added. It might take a second to show up." % feed_markup))
                        else:
                            flash(_(u"Feed %s added" % feed_markup))
        else:
            flash(_("Invalid url"), "error")

    discover = cool_feeds()

    return render_template("add_feed.html.j2", form=feed_form, feeds=feeds, discover=discover)


@cache.cached(timeout=60 * 60 * 24)
def cool_feeds():
    feeds = {
        _(u"Technology"): [
            _discover_feed("https://lwn.net/headlines/newrss"),
            _discover_feed("https://news.ycombinator.com/rss"),
            _discover_feed("https://techcrunch.com/feed/"),
            _discover_feed("http://feeds.arstechnica.com/arstechnica/technology-lab"),
            _discover_feed("https://github.com/isoteemu/jyu-demot/commits/master.atom"),
        ],
        _(u"Culture"): [
            _discover_feed("https://jezebel.com/rss"),
            _discover_feed("https://slutever.com/feed/atom"),
            _discover_feed("https://divamag.co.uk/feed"),
            _discover_feed("https://www.radiohelsinki.fi/feed/"),
            _discover_feed("https://balletbeautifulnyc.tumblr.com/rss"),
        ],
        _(u"Entertainmet"): [
            _discover_feed("https://feeds.yle.fi/uutiset/v1/majorHeadlines/YLE_URHEILU.rss"),
            _discover_feed("https://www.smbc-comics.com/comic/rss"),
            _discover_feed("https://nerdist.com/feed/"),
            _discover_feed("https://xkcd.com/atom.xml"),
            _discover_feed("https://www.youtube.com/feeds/videos.xml?channel_id=UCLx053rWZxCiYWsBETgdKrQ"),
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
    force = request.args.get("force", False)
    o.append("Force: %s" % force)

    for feed in Feed.query().fetch():
        if not force:
            schedule_feed_update_if_needed(feed)
        else:
            task = schedule_feed_update(feed)
            o.append(u"Scheduling feed %s for update at %s" % (repr(feed.url), task.eta))

    return u"\n".join(o), 202


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

    app.logger.info("Updating feed %s -> %s", repr(feed.title), repr(feed.url))
    # run actual updater.
    update_feed(feed)
    return ""


# @ndb.transactional(xg=True)
def update_feed(feed):
    # TODO: Check failure
    r = crawler().get(feed.url)
    r.raise_for_status()

    update_feed_with_rss(feed, r.content)
    feed.updated = datetime.utcnow()

    articles = update_feed_articles_with_rss(feed, r.content)

    memcache_key = memcache_key_latest % feed.key.id()
    memcache.delete(memcache_key)

    update_feed_interval(feed)

    # Collect assets.
    assets = filter(None, [get_entity_asset(e) for e in articles + [feed]])

    ndb.put_multi([feed] + articles + assets)


def feed_subscride(feed, user=None):
    r"""
    Subscribes user into feed. Creates new subscription object.

    :return: :class:`Subscription`
    """

    if not user:
        user = get_current_user()

    key = Subscription.generate_key(user, feed)

    entity = get_by_key(key)
    if not entity:
        entity = Subscription(key=key)

    entity.feed = feed.key
    entity.user = user.key
    entity.title = feed.title

    memcache_key = memcache_key_subscriptions % user.key.id()
    memcache.delete(memcache_key)

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
        app.logger.debug(u"Looking url %s for feeds.", repr(url))
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

    key = ndb.key.Key(Feed, url)
    feed = get_by_key(key) or Feed(key=key)

    kwargs['url'] = url
    kwargs.setdefault("type", "feedparser")

    feed.populate(**kwargs)
    return feed


def article_factory(feed, id=None, **kwargs):
    if not id:
        if kwargs.get("url", None):
            id = kwargs["url"]
        elif kwargs.get("title", None):
            app.logger.debug(u"Generating pseudo ID for %s: %s", feed.title, kwargs['title'])
            content = u"{title}|{published}".format(**kwargs)
            id = unicode(uuid.uuid5(namespace=uuid.NAMESPACE_OID, name=content.encode("utf-8")))
        else:
            raise KeyError("Missing feed ID, or suitable params for ID generation.")

    key = ndb.key.Key(Article, id, parent=feed.key)

    article = get_by_key(key) or Article(key=key, feed=feed.key)

    article.populate(**kwargs)
    return article


def saved_factory(article, user, **kwargs):
    r"""
    Create :class:`Saved` entity for article.

    :param article: Associated :class:`Article` instance.
    :param user: Associated :class:`User` instance.
    """

    if not isinstance(article, Article):
        raise TypeError("article needs to be instance of `Article`")

    if not isinstance(user, User):
        raise TypeError("`user` needs to be instance of `User`")

    kwargs['article'] = article.key
    kwargs['user'] = user.key

    key = Saved.generate_key(user.key, article.key)

    saved = get_by_key(key) or Saved(key=key, user=user.key, article=article.key)
    saved.populate(**kwargs)

    return saved


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
        title = feed_parser['feed'].get("title", title)
        subtitle = feed_parser['feed'].get('subtitle', "")
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
        feed.link = feed_parser['feed'].get("link")

    if 'image' in feed_parser['feed'] and 'href' in feed_parser['feed']['image']:
        # Create asset entity from feed image. Thease are usually poor quality.
        img = feed_parser['feed']['image']
        asset_factory(img.get("href"), feed, width=img.get("width", None), height=img.get("height", None), weight=20)

    if valid_url(feed_parser['feed'].get("logo", "")):
        asset_factory(feed_parser['feed'].get("logo"), feed, weight=10)


def update_feed_articles_with_rss(feed, content):
    """
    Parsers RSS feed content and update associated articles.

    :param feed: Feed entity.
    :param content: RSS feed as text.
    """

    articles = []
    feed_parser = feedparser.parse(BytesIO(content))

    app.logger.debug("Found %d entries for feed %s", len(feed_parser['entries']), feed.url)

    for article in feed_parser.get("entries", []):

        id = None
        title = ""
        content = ""
        url = ""
        published = None
        try:
            # Find textual stuff
            id = article.get("id", None)

            title = Markup(article.get("title", "")).striptags()
            url = article.get("link")

            updated = article.get("published_parsed") or article.get("updated_parsed")
            published = datetime(*updated[:6])

            if "content" in article:
                if type(article['content']) == list:
                    content = article['content'][0].get("value")
                else:
                    content = article['content'].get("value")
            else:
                content = article.get("summary", "")

            article_entity = article_factory(feed, id=id, title=title, url=url,
                                             published=published, content=content)

            articles.append(article_entity)

            # Asset searching
            # ===============

            for media in article.get("media_thumbnail", []):
                # Look for thumbnails. Thease are usually quite small
                if not valid_url(media.get("url", "")):
                    continue

                asset_factory(media['url'], article_entity, weight=20)
                app.logger.debug("Found media thumbnail for article: %s", media['url'])

            for media in article.get("media_content", []):
                # media content can be pretty much anything. Only lookup things that
                # have been declared with "image/*"  type.
                if not valid_url(media.get("url", "")):
                    continue

                if not media.get("type", "").lower().startswith("image/"):
                    app.logger.debug(u"Found media content, but its not declared as image: %s" % repr(media['url']))
                    continue

                asset_factory(media['url'], article_entity, weight=50)
                app.logger.debug("Found media content for article: %s", media['url'])

            for media in article.get("links", []):
                # Look for enclosure links
                if not valid_url(media.get("href", "")):
                    continue
                if not media.get("rel", "").lower() == "enclosure":
                    continue

                if not media.get("type", "").lower().startswith("image/"):
                    app.logger.debug(u"Found link content, but its not declared as image: %s" % repr(media['href']))
                    continue

                asset_factory(media['href'], article_entity, weight=50)
                app.logger.debug("Found media thumbnail for article: %s", media['href'])

            for asset in extract_assets(content):
                # Try extracting content from article contents.
                asset_url, asset_weight = asset
                asset_factory(asset_url, article_entity, weight=asset_weight)

        except Exception as e:
            app.logger.exception(e)

    return articles


def update_feed_interval(feed):
    r"""
    Calculates average interval between articles.
    """
    previous = datetime.utcnow()
    time = timedelta()

    articles = Article.query(ancestor=feed.key).order(-Article.published).fetch(default_limit)

    i = 0
    for article in articles:
        if article.published == previous:
            continue
        i += 1
        time += previous - article.published
        previous = article.published

    if i == 0:
        app.logger.debug("Feed %s had no suitable articles.", repr(feed.url))
        return None

    interval = time / i
    interval = interval.total_seconds() / 60

    # Make sure interval is between 30min - 1.5days
    interval = min(60 * 24 * 1.5, interval)
    interval = max(30, interval)

    feed.update_inteval = int(interval)

    app.logger.debug(u"Set feed %s update interval to %d", feed.url, interval)


def filter_content(html):
    return Markup(html).striptags()


def scrape_logo(soup):
    """
    Look for logo from webpage.
    """

    app.logger.debug("Scraping logo")

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
