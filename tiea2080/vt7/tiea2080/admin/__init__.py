# -*- coding: utf-8 -*-

from __future__ import unicode_literals

from flask import (
    Blueprint,
    Markup,
    flash,
    render_template,
    redirect,
    Response,
    url_for,
    request,
    current_app as app,
)
from flask_babel import _

from google.appengine.api import memcache

from io import BytesIO
import feedparser

from tiea2080 import (
    crawler
)
from tiea2080.models import (
    ndb,
    Feed,
    User,
    get_by_key,
)
from tiea2080.feed import update_feed

bp = Blueprint('admin', __name__, template_folder='templates')


def init_app(app):
    app.register_blueprint(bp, url_prefix="/admin")
    app.config.setdefault("DEBUG", True)


@bp.route("/feeds", methods=('POST', 'GET'))
def page_list_feeds():
    feeds = []

    if request.method == "POST":
        if "force-refresh" in request.form:
            feed_to_refresh = get_by_key(ndb.key.Key(urlsafe=request.form["force-refresh"]))
            flash(_("Refreshing feed %(feed)s", feed=Markup(feed_to_refresh.title).striptags()))
            update_feed(feed_to_refresh)

        if "delete-feed" in request.form:
            feed = ndb.key.Key(urlsafe=request.form["delete-feed"]).get()

            flash(_("Removed feed %(feed)s", feed=Markup(feed.title).striptags()))
            app.logger.info("Deleted feed %s", repr(feed))

            feed.delete()
            memcache.flush_all()


    feed_q = Feed.query()
    for feed in feed_q:
        data = {
            'title': feed.title,
            'url': feed.url,
            'key': feed.key.urlsafe(),
            'articles': [],
        }

        for article in feed.articles():
            data['articles'].append({
                'title': article.title,
            })

        feeds.append(data)

    return render_template("feeds.html.j2", feeds=feeds)

@bp.route("/feedparser/<key>")
def page_debug_feedparser(key):
    from pprint import pformat

    feed = get_by_key(ndb.key.Key(urlsafe=key))

    r = crawler().get(feed.url)
    feed_parser = feedparser.parse(BytesIO(r.content))

    return Response(pformat(feed_parser), mimetype='text/plain')



@bp.route("/delete-user")
def page_delete_user():
    user_id = request.args.get("user")
    if user_id:
        user = ndb.key.Key(urlsafe=user_id).get()
        user.delete()
        memcache.flush_all()

    msg = []
    for user in User.query().fetch():
        msg.append(u"""<a href="{url}?feed={user}">{uid} {email}</a>""".format(
            url=url_for('.page_delete_user'),
            user=user.key.urlsafe(),
            uid=user.key.id(),
            email=user.email
        ))

    return u"<br>".join(msg)
