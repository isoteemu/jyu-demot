# -*- coding: utf-8 -*-

from flask import Blueprint, redirect, request

from google.appengine.api import users

from .models import User, ndb

current_user = None

bp = Blueprint("user", __name__)

def init_app(app):
    with app.app_context():
        app.register_blueprint(bp)

        current_user = get_current_user()

        app.jinja_env.globals.update(login_link=login_link, current_user=current_user)


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


