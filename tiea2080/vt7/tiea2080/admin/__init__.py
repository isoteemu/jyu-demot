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
    jsonify,
    current_app as app,
)
from flask_babel import _

from google.appengine.api import memcache

from io import BytesIO
# import feedparse
import logging
import os

import feedparser

from tiea2080 import (
    crawler,
)
from tiea2080.user import (
    get_current_user,
)
from tiea2080.utils import (
    html_parser,
)
from tiea2080.models import (
    Feed,
    Subscription,
    User,
    ndb,
    get_by_key,
)
from tiea2080.feed import update_feed


class FlashHandler(logging.StreamHandler):
    def emit(self, record):
        if get_current_user().is_admin:
            msg = self.format(record)
            name = os.path.basename(record.pathname)
            line = record.lineno
            flash(Markup(u"<var>%s</var>[<var>%d</var>]: %s") % (name, line, msg), record.levelname)


bp = Blueprint('admin', __name__, template_folder='templates')

def init_app(app):

    with app.app_context():
        app.logger.addHandler(FlashHandler())
        app.config.setdefault("DEBUG", True)

    app.register_blueprint(bp, url_prefix="/admin")


@bp.route("/test", methods=('POST', 'GET'))
def test():

    from tiea2080.extractors import extract_assets
    #feed = get_by_key(ndb.key.Key(Feed, 'https://www.reddit.com/r/bois/.rss'))

    return repr(extract_assets("""<a href="https://streamja.com/L9GA">foo</a>"""))

    return soup



@bp.route("/feeds", methods=('POST', 'GET'))
def page_list_feeds():
    feeds = []

    if request.method == "POST":
        if "force-refresh" in request.form:
            feed_to_refresh = get_by_key(ndb.key.Key(urlsafe=request.form["force-refresh"]))
            flash(_("Refreshing feed %(feed)s", feed=Markup.escape(feed_to_refresh.title)))
            update_feed(feed_to_refresh)

        if "delete-feed" in request.form:
            feed = ndb.key.Key(urlsafe=request.form["delete-feed"]).get()

            flash(_("Removed feed %(feed)s", feed=Markup.escape(feed.title)))
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
                'published': article.published
            })

        feeds.append(data)

    return render_template("feeds.html.j2", feeds=feeds)


@bp.route("/users", methods=('GET', 'POST'))
def page_users():

    if request.method == "POST":
        if "remove" in request.form:
            ndb.key.Key(urlsafe=request.form.get("remove")).get().delete()
            flash(_("User removed"))

    users = User.query().fetch()
    return render_template("users.html.j2", users=users)


@bp.route("/remove-orphan")
def page_remove_orphan():
    removed = remove_orphan()

    return jsonify(removed)


def remove_orphan():
    r"""
    Removes orpan entitys.
    """
    delete = []

    for sub in Subscription.query().fetch():
        if not sub.feed.get() or not sub.user.get():
            delete += sub.key

    return ndb.delete_multi_async(delete)


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
        app.logger.info("User deleted")
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
