#!/usr/bin/python2
# -*- coding: utf-8 -*-

from flask import current_app as app, session
from google.appengine.ext import ndb
from google.appengine.api import memcache
from google.appengine.api import images

import random
import uuid

# Factories should store models into model_storage.
# TODO: tämän pitäisi olla request kontekstissa.
model_storage = {}
memcache_namespace = u"%s" % __name__

memcache_key_subscriptions = "subscribed:%s"

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
        key = self.key
        if key in model_storage:
            del model_storage[key]

        memcache.delete(repr(key), namespace=memcache_namespace)
        self.key.delete_async()

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
    blob_key = ndb.StringProperty()

    created = ndb.DateTimeProperty(auto_now_add=True)
    updated = ndb.DateTimeProperty(auto_now=True)

    weight = ndb.IntegerProperty()

    def __init__(self, *args, **kwargs):
        r = super(Asset, self).__init__(*args, **kwargs)

        if self.key:
            parent = self.key.parent()

            if parent:
                # Tell about self to parent instance.
                get_by_key(parent).tell_about_asset(self)

        return r

    def delete(self):
        if self.blob_key:
            images.delete_serving_url_async(self.blob_key)

        return super(Asset, self).delete()


class AssetedModel(Model):

    _asset_memcache_key = "Asset-for:%s"

    def __init__(self, *args, **kwargs):
        # Preferred asset
        self._asset = None

        super(AssetedModel, self).__init__(*args, **kwargs)

    def Asset(self):
        r"""
        Returns associated asset.
        """
        memcache_key = self._asset_memcache_key % repr(self.key)

        if not self._asset:
            self._asset = memcache.get(memcache_key, namespace=memcache_namespace)

        if not self._asset:
            # Fetch only key.
            asset = Asset.query(ancestor=self.key).order(-Asset.weight).get()
            if asset:
                self._asset = asset.key
                memcache.set(memcache_key, self._asset, namespace=memcache_namespace)

        if self._asset:
            return get_by_key(self._asset)

        return None

    def tell_about_asset(self, asset):
        r"""
        Tell AssetedModel about new/changed asset. Updates internal pointer and memcache.
        """
        old_asset = self.Asset()
        if not old_asset or old_asset.weight < asset.weight:
            # If old asset is lighter, update to new, preferred asset.
            self._asset = asset.key

            memcache_key = self._asset_memcache_key % repr(self.key)
            memcache.set(memcache_key, self._asset, namespace=memcache_namespace)

    def delete(self, key, future):
        q = Asset.query(ancestor=key)
        ndb.multi_delete_async(q.fetch())
        return super(AssetedModel, self).delete()


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

    def delete(self):
        delete = []

        # Collect all subscriptions and articles for deletion
        delete.append(Subscription.query(Subscription.feed == self.key).fetch())
        delete.append(Article.query(Article.feed == self.key).fetch())
        delete.append(Saved.query(Saved.feed == self.key).fetch())

        ndb.multi_delete_async(delete)

        super(Feed, self).delete()

    def user_subscribed(self, user):
        raise DeprecationWarning("Replaced by `User.has_subscribed()`")

    def articles(self):
        return Article.query(
            Article.feed == self.key
        ).fetch()


class User(Model):
    r"""
    Key is google user key.
    """

    email = ndb.StringProperty()
    is_authenticated = False

    nickname = "Anonymous"

    seen = ndb.DateTimeProperty(auto_now=True)

    def Subscriptions(self):
        return Subscription.query(Subscription.user == self.key).fetch()

    def Saved(self):
        return Saved.query(Saved.user == self.key).fetch()

    def has_saved(self, article):
        r"""
        Return :class:`Saved` instance for :param:`article`.

        Note that it does NOT return :type:`bool`, but rather `None` or `Saved`

        :return: :class:`Saved` instance if :param:`article` is saved, ``None`` if not.
        """

        key = Saved.generate_key(self.key, article.key)
        return get_by_key(key)

    def has_subscribed(self, feed):
        key = Subscription.generate_key(self, feed)
        return get_by_key(key)

    def delete(self):
        delete = []
        key = self.key

        # Collect items for deletion
        delete.append(Subscription.query(Subscription.user == key).fetch())
        delete.append(Notification.query(ancestor=key).fetch())
        delete.append(Saved.query(ancestor=key).fetch())

        super(User, self).delete()


class Notification(ndb.Model):
    r"""
    Notifications are attached to :class:`User` as parent.
    """
    _new = False

    message = ndb.TextProperty()
    category = ndb.StringProperty()

    timestamp = ndb.DateTimeProperty(auto_now_add=True)

    def delete(self):
        return self.key.delete()


class Article(AssetedModel):
    id = ndb.StringProperty()
    title = ndb.StringProperty()
    url = ndb.StringProperty(indexed=False)
    content = ndb.TextProperty()
    published = ndb.DateTimeProperty(required=True)
    feed = ndb.KeyProperty(kind=Feed)


class Subscription(Model):
    r"""
    Keys are formed by :class:`Feed` id as id and :class:`User` as parent.
    """
    title = ndb.StringProperty(indexed=False)
    created = ndb.DateTimeProperty(auto_now_add=True)

    feed = ndb.KeyProperty(kind=Feed, required=True)
    user = ndb.KeyProperty(kind=User, required=True)

    weight = ndb.IntegerProperty()

    def Feed(self):
        return get_by_key(self.feed)

    @staticmethod
    def generate_key(user, feed):
        r"""
        Generate subscription key.
        """
        user_key = get_entity_key(user)
        feed_key = get_entity_key(feed)

        key = ndb.key.Key(Subscription, feed_key.id(), parent=user_key)
        return key

    def delete(self):
        memcache.delete(memcache_key_subscriptions % self.user.id())
        return super(Subscription, self).delete()


class Saved(Model):
    article = ndb.KeyProperty(kind=Article, required=True)
    user = ndb.KeyProperty(kind=User, required=True)

    weight = ndb.IntegerProperty()

    created = ndb.DateTimeProperty(auto_now_add=True)

    def Article(self):
        return get_by_key(self.article)

    @staticmethod
    def generate_key(user, article):
        r"""
        Generate suitable key instance.
        """
        if not isinstance(user, ndb.Key) or not isinstance(article, ndb.Key):
            raise TypeError("`user` and `article` needs to be key instances.")

        # use article id for own id, and make user as parent.
        return ndb.key.Key(Saved, article.id(), parent=user)


def subscription_key(user, feed):
    raise DeprecationWarning("Replaced by Subscription.generate_key")


def init_app(app):
    with app.app_context():
        app.teardown_appcontext(store_models)


def get_entity_key(entity):
    r"""
    Return key instance.
    """
    key = None
    if isinstance(entity, ndb.Key):
        key = entity
    elif isinstance(entity, ndb.Model):
        key = entity.key
    else:
        raise TypeError("Entity needs to be either ndb.Key or ndb.Model")

    return key


def store_models(exception):
    """
    On teardown, store temporary objects.
    """
    entities = {repr(k): v for k, v in model_storage.items()}
    memcache.set_multi(entities, namespace=memcache_namespace)


def get_by_key(key, only_local=False):
    r"""
    Retrieve entity by key.
    """
    if not key:
        return None

    # Look for entity in different storages
    if key in model_storage:
        # Entity already in current context.
        pass
    else:
        # Look for entity in memcache
        mem = memcache.get(repr(key), namespace=memcache_namespace)
        if mem:
            model_storage[key] = mem
        elif not only_local:
            # Try loading entity from datastore
            ent = key.get()
            if ent:
                model_storage[key] = ent
                # Item is not stored yet on memcache. It's triggered when
                # request context is in teardown phase.

    if key in model_storage:
        return model_storage[key]

    return None

