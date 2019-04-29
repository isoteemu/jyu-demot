#!/usr/bin/python2
# -*- coding: utf-8 -*-

from flask import current_app as app, session
from google.appengine.ext import ndb
from google.appengine.api import memcache

import random
import uuid

# Factories should store models into model_storage.
model_storage = {}
memcache_namespace = u"%s" % __name__

r"""
MODELS
======
"""


class Model(ndb.Model):
    r"""
    Model which hooks into temporary object storage
    """

    def __init__(self, *args, **kwargs):
        r = super(Model, self).__init__(*args, **kwargs)
        if self.key:
            model_storage[self.key] = self
        return r

    def delete(self):
        if self.key in model_storage:
            del model_storage[self.key]
            memcache.delete(repr(self.key), namespace=memcache_namespace)

        self.key.delete()

    def _post_put_hook(self, future):
        memcache.set(repr(self.key), self, namespace=memcache_namespace)


class Asset(Model):
    url = ndb.StringProperty()
    # Original image width and height
    width = ndb.IntegerProperty()
    height = ndb.IntegerProperty()

    # Snippet contains 10x10px presentation of image in base64 encoded format.
    snippet = ndb.TextProperty(indexed=False)
    data = ndb.BlobProperty()

    created = ndb.DateTimeProperty(auto_now_add=True)
    weight = ndb.IntegerProperty()

    def __init__(self, *args, **kwargs):
        r = super(Asset, self).__init__(*args, **kwargs)

        # If entity has parent, add self into its hidden asset stash.
        if self.key:
            get_by_key(self.key.parent())._assets.append(self.key)

        return r

    def put(self, *args, **kwargs):
        return super(Asset, self).put(*args, **kwargs)


class AssetedModel(Model):
    def __init__(self, *args, **kwargs):
        self._assets = []
        super(AssetedModel, self).__init__(*args, **kwargs)

    def put(self, *args, **kwargs):
        super(AssetedModel, self).put(*args, **kwargs)
        for asset_key in self._assets:
            get_by_key(asset_key).put()


class Feed(AssetedModel):

    url = ndb.StringProperty()
    title = ndb.StringProperty(required=True)
    type = ndb.StringProperty(required=True)
    link = ndb.StringProperty()

    updated = ndb.DateTimeProperty()
    # How long between updates in minutes. In perfect world this would be automaticly
    # adjusted.
    update_inteval = ndb.IntegerProperty(default=480)

    def __init__(self, *args, **kwargs):
        self._articles = []
        super(Feed, self).__init__(*args, **kwargs)

    def put(self, *args, **kwargs):
        super(Feed, self).put(*args, **kwargs)
        for article in self._articles:
            get_by_key(article).put()

    def user_subscribed(self, user):
        r"""
        Return ``True`` if currently logged in user is subscribed.
        """
        subscription = subscription_key(user, self).get()
        if not subscription:
            return False
        else:
            return True


class User(Model):
    email = ndb.StringProperty()
    is_authenticated = False


class Article(AssetedModel):
    title = ndb.StringProperty()
    url = ndb.StringProperty(indexed=False)
    content = ndb.TextProperty()
    published = ndb.DateTimeProperty(required=True)
    feed = ndb.KeyProperty(kind=Feed)

    def __init__(self, *args, **kwargs):
        r = super(Article, self).__init__(*args, **kwargs)

        # If entity has parent, add self into its hidden asset stash.
        if self.key:
            get_by_key(self.key.parent())._articles.append(self.key)
        return r


class Subscription(Model):
    r"""
    Keys are formed by :class:`Feed` and :class:`User`.
    """
    title = ndb.StringProperty(indexed=False)
    created = ndb.DateTimeProperty(auto_now_add=True)

    feed = ndb.KeyProperty(kind=Feed, required=True)
    user = ndb.KeyProperty(kind=User, required=True)

    weight = ndb.IntegerProperty()


def subscription_key(user, feed):
    r"""
    Generate subscription key.
    """

    key_str = "%s|%s" % (repr(user.key.id()), repr(feed.key.id()))
    id = uuid.uuid5(namespace=uuid.NAMESPACE_OID, name=key_str.encode("utf-8"))
    return ndb.key.Key(Subscription, str(id))


def init_app(app):
    with app.app_context():
        app.teardown_appcontext(teardown)


def teardown(exception):
    """
    On teardown, store temporary objects.
    """
    entities = {repr(k): v for k, v in model_storage.items()}
    memcache.set_multi(entities, namespace=memcache_namespace)


def get_by_key(key):
    r"""
    Retrieve entity by key.
    """

    # Look for entity in different storages
    if key in model_storage:
        # Entity already in current context.
        pass
    else:
        # Look for entity in memcache
        mem = memcache.get(repr(key), namespace=memcache_namespace)
        if mem:
            model_storage[key] = mem
        else:
            # Try loading entity from datastore
            ent = key.get()
            if ent:
                model_storage[key] = ent

    if key in model_storage:
        return model_storage[key]

    return None

