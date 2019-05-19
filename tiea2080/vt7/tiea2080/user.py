# -*- coding: utf-8 -*-

from __future__ import unicode_literals

# TODO: Muuta check_user_access() -decoraattoriksi.

from flask import (
    Blueprint,
    g,
    redirect,
    abort,
    flash,
    request,
    url_for,
    jsonify,
    current_app as app,
    render_template,
    get_flashed_messages
)

from flask import _request_ctx_stack
from flask_babel import _
from flask_gravatar import Gravatar

from google.appengine.api import users

from datetime import datetime

from .models import (
    User,
    Notification,
    Subscription,
    Saved,
    ndb,
    get_by_key
)
from . import (
    memcache,
    csrf,
)
from .subsctiptions import (
    user_subscriptions,
    unsubscribe,
    memcache_key_subscriptions,
)

gravatar = None

bp = Blueprint("user", __name__)


def init_app(app):
    app.register_blueprint(bp)
    gravatar = Gravatar(app,
                        size=150,
                        rating='r'
                        )
    app.teardown_appcontext(_save_last_seen)

    with app.app_context():

        app.jinja_env.globals.update(
            login_link=login_link,
            get_notifications=get_notifications,
            get_flashed_messages=get_flashed_messages_override
        )

        app.context_processor(_get_current_user)


def _get_current_user():
    return dict(current_user=get_current_user())


def get_current_user():
    r""" Return current user Model.
    """

    current_user = g.get('current_user', None)
    if current_user:
        return current_user

    user = User()

    try:
        framework_user = users.get_current_user()
        id = u"%s" % framework_user.user_id()

        user = User.get_or_insert(id)

        email = framework_user.email()
        if email and email != user.email:
            user.email = email
            user.put()
        elif not email:
            user.email = ""

        user.is_authenticated = True
        user.is_admin = True if users.is_current_user_admin() else False
        user.nickname = framework_user.nickname() or _("Anonymous")

    except (users.UserNotFoundError, AttributeError):
        pass

    g.current_user = user

    return user


def check_user_access(email):
    user = get_current_user()

    if user.email != email:
        app.logger.warning("Access denied. Espected email %s, got %s", user.email, email)
        # Show only current user.
        abort(403)


@bp.route("/<email>/")
def page_profile(email):
    r"""
    User profile page.
    """
    user = get_current_user()
    check_user_access(email)

    subscriptions = user_subscriptions(user)
    saved = list(Saved.query(Saved.user == user.key).fetch())
    saved.sort(key=lambda x: x.weight or x.Article().title)

    return render_template("user.html.j2", subscriptions=subscriptions, saved=saved)


@bp.route("/<email>/subscriptions.json", methods=('GET', 'POST'))
def subscription_weights_json(email):
    user = get_current_user()
    check_user_access(email)

    status = {"status": "OK"}
    subs = user_subscriptions(user)

    dirty = []

    if request.method == "POST":
        try:
            for sub in subs:
                # Update weights if weight is changed
                weight = request.form.get(sub.feed.urlsafe(), None)
                if weight and weight.isdigit():
                    weight = int(weight)
                    if sub.weight != weight:

                        sub.weight = int(weight)
                        dirty.append(sub)

            if len(dirty):
                # If weight has changed, update them into datastore
                ndb.put_multi(dirty)

                # Force refreshing memstore
                user_subscriptions(user, force_refresh=True)

        except Exception as e:
            app.logger.exception(e)
            status['status'] = "FAIL"
            status['message'] = u"%s" % e

    return jsonify(status)


@bp.route("/<email>/saved.json", methods=('GET', 'POST'))
def saved_weights_json(email):
    user = get_current_user()
    check_user_access(email)

    status = {"status": "OK"}
    saved = Saved.query(Saved.user == user.key).fetch()
    dirty = []

    if request.method == "POST":
        try:
            for sub in saved:
                # Update weights if weight is changed
                weight = request.form.get(sub.key.urlsafe(), None)
                if weight and weight.isdigit():
                    weight = int(weight)
                    if sub.weight != weight:

                        sub.weight = int(weight)
                        dirty.append(sub)

            if len(dirty):
                # If weight has changed, update them into datastore
                ndb.put_multi(dirty)

        except Exception as e:
            app.logger.exception(e)
            status['status'] = "FAIL"
            status['message'] = u"%s" % e

    return jsonify(status)


@bp.route("/<email>/delete-account", methods=('POST',))
def page_delete_account(email):
    user = get_current_user()

    check_user_access(email)

    app.logger.info("User account will be deleted: %s", user.email)
    deletion_url = url_for('static', filename='account-deleted.html')

    user.is_authenticated = False
    user.delete()

    return redirect(users.create_logout_url(deletion_url), code=303)


@bp.route("/<email>/feed", methods=('POST', 'GET'))
def modify_subscription(email):
    r"""
    Page for modifying subscription info.
    """

    user = get_current_user()
    check_user_access(email)

    sub_id = request.form.get("subscription") or request.args.get("subscription") or abort(404)

    subscription = get_by_key(ndb.key.Key(urlsafe=sub_id))

    if not subscription:
        abort(404)

    if request.method == "POST":
        action = request.form.get("action", None)
        feed_removed = False
        try:
            if action == "save":
                subscription.title = request.form.get("title", subscription.title)
                subscription.put()
                user_subscriptions(user, force_refresh=True)
                flash(_("Feed updated"))
            elif action == "remove":
                unsubscribe(subscription)
                flash(_("Feed removed."))
                feed_removed = True

        except Exception as e:
            flash(_("Could not save feed"), "error")
            app.logger.exception(e)

        if feed_removed:
            return redirect(url_for("user.page_profile", email=user.email))

    return render_template("modify-subscription.html.j2", subscription=subscription)


def login_link():
    return users.create_login_url('/')


@bp.route("/logout", methods=('POST',))
def page_logout():
    url = url_for('static', filename='account-logout.html')
    return redirect(users.create_logout_url(url), code=303)


r"""
NOTIFICATION
============
"""


@bp.route("/notifications")
def page_notifications():
    # TODO: Implement
    if not app.debug:
        abort(404)
    notifications = get_notifications()
    return repr(notifications)


def get_notifications(limit=None):
    r"""
    Return all notifications for user, sorted by recency.

    Looks also for :class:`Flask.helper.flash` messages.
    """
    if hasattr(_request_ctx_stack.top, "user_notifications"):
        # Fetch notifications only once.
        return _request_ctx_stack.top.user_notifications

    notifications = []

    memcache_key = None
    user = get_current_user()
    if user.is_authenticated:
        memcache_key = "user.notifications:%s" % user.key.id()
        cached_notifications = memcache.get(memcache_key)

        if cached_notifications:
            notifications = notifications + cached_notifications
        else:
            q = Notification.query(ancestor=user.key).order(-Notification.timestamp)
            for n in q.fetch(limit=limit):
                notifications.append(n)

    flashes = get_flashed_messages(with_categories=True)

    if flashes:
        memcache.delete(memcache_key)
        new_notifications = []
        for category, message in flashes:
            notification_entity = Notification(
                parent=user.key,
                message=message, category=category, timestamp=datetime.utcnow()
            )
            notification_entity._new = True
            new_notifications.append(notification_entity)

        notifications = new_notifications + notifications
        if user.is_authenticated:
            ndb.put_multi(new_notifications)

    if user.is_authenticated:
        memcache.set(memcache_key, notifications)

    _request_ctx_stack.top.user_notifications = notifications

    return notifications[0:limit]


def get_flashed_messages_override(with_categories=False, category_filter=[]):
    r"""
    Replacement function for :func:`Flask.helpers.get_flashed_messages`.
    """

    messages = get_notifications()
    messages = [m for m in messages if m._new is True]

    if category_filter:
        messages = list(filter(lambda f: f.category in category_filter, messages))

    if with_categories:
        return [(m.category, m.message) for m in messages]

    return [m.message for m in messages]


def _save_last_seen(exception):
    """
    Hook to save user object when request is completed.
    This is to trigger saving `User.seen` property.
    """

    user = get_current_user()
    if user.is_authenticated:
        user.seen = datetime.utcnow()
        user.put_async()
