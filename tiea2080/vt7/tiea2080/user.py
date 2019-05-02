# -*- coding: utf-8 -*-

from flask import Blueprint, redirect, request, current_app as app, get_flashed_messages

from flask import flash

from google.appengine.api import users

from datetime import datetime

from .models import User, Notification, ndb
from . import memcache

current_user = None

bp = Blueprint("user", __name__)


def init_app(app):
    app.register_blueprint(bp)
    with app.app_context():

        current_user = get_current_user()
        app.jinja_env.globals.update(
            login_link=login_link, current_user=current_user,
            get_notifications=get_notifications,
            get_flashed_messages=get_flashed_messages_override
        )


def get_current_user():
    r""" Return current user Model.
    """
    if current_user:
        return current_user

    user = User()
    try:
        framework_user = users.get_current_user()
        id = u"%s" % framework_user.user_id()

        user = User.get_or_insert(id)

        email = framework_user.email()
        if email != user.email:
            user.email = email
            user.put()

        user.is_authenticated = True

    except (users.UserNotFoundError, AttributeError):
        pass

    return user


def login_link():
    print(request.base_url)
    users.create_login_url('/')


@bp.route("/logout", methods=('POST',))
def page_logout():
    url = ""
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


def get_notifications(offset=0, limit=None):
    r"""
    Return all notifications for user, sorted by recency.

    Looks also for :class:`Flask.helper.flash` messages.
    """

    if offset:
        raise NotImplementedError("Implement limit and offset")
    notifications = []

    user = get_current_user()

    memcache_key = "user.notifications:%s" % user.key.id()
    cached_notifications = memcache.get(memcache_key)

    if cached_notifications:
        notifications = cached_notifications + notifications
    else:
        print("!!! REFRESH")
        q = Notification.query(ancestor=user.key).order(-Notification.timestamp)
        for n in q.fetch(offset=offset, limit=limit):
            notifications.append(n)

    if offset == 0:
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

            ndb.put_multi(new_notifications)
            notifications = new_notifications + notifications
            memcache.set(memcache_key, notifications)

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

