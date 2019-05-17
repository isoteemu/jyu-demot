#!/usr/bin/python2
# -*- coding: utf-8 -*-

from flask import g, request, current_app as app
from flask_wtf import csrf
from bs4 import BeautifulSoup
import requests
from url_normalize import url_normalize
from urlparse import urlparse
from urllib import urlencode
from datetime import datetime, timedelta
from html_sanitizer.sanitizer import Sanitizer

from . import __title__, __version__, __url__



class CSRFProtect(csrf.CSRFProtect):
    r"""
    Cookie addon for Flask-WTF CSRFProtect.
    """
    def init_app(self, app):
        r = super(CSRFProtect, self).init_app(app)

        app.after_request(self.set_csrf_cookie)
        return r

    def _get_csrf_token(self):
        r"""
        Look for CSRF token from cookie
        """

        token = super(CSRFProtect, self)._get_csrf_token()
        if token is None:
            name = self._get_cookie_name(app)
            token = request.cookies.get(name, None)

        return token

    def _get_cookie_name(self, app):
        name = app.config.get("WTF_CSRF_HEADERS", "X-CSRFToken")
        if isinstance(name, (list, tuple)):
            name = name[0]

        return name

    def set_csrf_cookie(self, response):
        r"""
        Inject csrf cookie into :param:`response`.
        """
        if not app.config.get("WTF_CSRF_ENABLED", False):
            return

        name = self._get_cookie_name(app)

        expires_seconds = app.config.get("WTF_CSRF_TIME_LIMIT", 3600)
        expires = datetime.utcnow() + timedelta(seconds=expires_seconds)

        response.set_cookie(name, csrf.generate_csrf(), expires=expires, httponly=True)
        return response


def html_parser(*args, **kwargs):
    r"""
        Wrapper for BeatifulSoup
    """
    kwargs.setdefault("features", "html.parser")
    return BeautifulSoup(*args, **kwargs)


def crawler():
    r"""
        Wrapper for ``requests``

        Sets global singleton ``g.crawler`` for :class:`requests.Session()`
    """

    if 'crawler' not in g:
        g.crawler = requests.Session()
        g.crawler.headers.update({
            'User-Agent': crawler_user_agent()
        })

    return g.crawler


def valid_url(url):

    if not url:
        return False

    try:
        # TODO: Use url validator

        url_parsed = urlparse(url)
        own_parsed = urlparse(request.host_url)

        if url_parsed.netloc == "":
            app.logger.debug(u"Url is missing ``netloc``: %s", repr(url))
            return False

        if url_parsed.scheme not in ['http', 'https']:
            app.logger.debug(u"Unknown url scheme: %s", repr(url))
            return False

        # Check for not being on our domain.
        url_host = url_parsed.netloc.split(":")[0].lower()
        own_host = own_parsed.netloc.split(":")[0].lower()
        if own_host == url_host:
            app.logger.info(u"Rejected url pointing to self: %s", url)
            return False

    except Exception as e:
        app.logger.exception(e)
        return False

    return True


def crawler_user_agent():
    r"""
    Robot user agent string.
    """
    url = __url__
    try:
        if not app.debug:
            url = request.host_url
    except:
        pass

    return u'{title}/{version} ({url})'.format(**{
        'title': __title__,
        'version': __version__,
        'url': url
    })


def sanitize_html(html):

    sanitizer = Sanitizer()
    return sanitizer.sanitize(html)

    soup = html_parser(html)
    [template_tag.decompose() for template_tag in soup.findAll("template")]
    return unicode(soup)
