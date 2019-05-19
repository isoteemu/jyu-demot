# -*- coding: utf-8 -*-

from . import memcache
from .models import (
    Subscription,
    get_by_key,
    memcache_key_subscriptions,
    ndb,
)


def user_subscriptions(user, force_refresh=False):
    r"""
    Return :class:`Subscription`s user has subscribed into.

    :param user: :class:`User` model to look Subscriptions.
    :param force_refresh: Force refreshing memcache. Fetches
                          list of subscriptions from datastore,
                          and pushes local models into memcache.
    """

    # Try lookin subscriptions from memcache
    memcache_key = memcache_key_subscriptions % user.key.id()
    subscribed = None if force_refresh else memcache.get(memcache_key)

    if not subscribed or force_refresh:
        subscribed = []
        for sub in Subscription.query(Subscription.user == user.key).iter():
            # If local model_storage has subscription, use it.
            local_sub = get_by_key(sub.key, True)
            if local_sub:
                sub = local_sub

            subscribed.append(sub)

        # Sort them by weight, and by title second.
        subscribed.sort(key=lambda x: x.weight or x.title)

        # Store into memcache.
        memcache.set(memcache_key, subscribed)

    return subscribed


def unsubscribe(subscription):
    r"""
    Remove user :class:`Subscription`.

    Deletes object from datastore, and updates memcache.
    """

    user = get_by_key(subscription.user)

    subs = user_subscriptions(user)
    if subscription in subs:
        subs.remove(subscription)
        subscription.delete()

        memcache_key = memcache_key_subscriptions % user.key.id()
        memcache.set(memcache_key, subs)

    else:
        raise ValueError("Subscription not in subscriptions.")


