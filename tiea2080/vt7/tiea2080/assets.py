# -*- coding: utf-8 -*-

r"""
Asset handling.

TODO: Use blobstore

"""
from flask import Blueprint, send_from_directory, abort, current_app as app, render_template, url_for, redirect, make_response
import werkzeug.exceptions
from google.appengine.api import images
from base64 import b64encode
import os
from urllib import quote, unquote
import uuid

from . import memcache, logger
from .utils import url_normalize, valid_url, urlparse, crawler
from .models import Asset, AssetedModel, ndb, get_by_key, model_storage

from io import BytesIO

ASSET_SIZES = {
    'Feed': (96, 96),
    'Article': (200, 200),
    # Thease seems to be what instagram uses
    'square': (1080, 1080),
    'vertical': (1080, 1350),
    'horizontal': (1080, 566),
}
bp = Blueprint('assets', __name__)


def init_app(app):
    media_url = app.config.get('THUMBNAIL_MEDIA_URL', '/asset')
    if media_url[0] != "/":
        media_url = u"/%s" % media_url

    app.register_blueprint(bp, url_prefix=media_url)

    app.jinja_env.filters.update(
        asset_img=asset_img
    )


def asset_img(entity, size=None):
    r"""
    Jinja filter for displaying asset image

    ..usage:
        >>> {{ entity|asset_img }}

    """

    if not entity:
        app.logger.warning(u"Can't create image; Entity is missing.")
        return u'<img src="data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7" class="missing-entity" /><!-- Entity missing -->'

    if size is None:
        size = entity._get_kind()

    if size not in ASSET_SIZES:
        raise ValueError("Asset size ``%s`` not found for asset." % size)

    width, height = get_asset_dimensions(entity, size)
    params = {
        'src': "",
        'width': width,
        'height': height,
        'alt': entity.title
    }

    asset = get_entity_asset(entity)

    # TODO: Recurse into parents.
    if asset:
        params['src'] = url_for("assets.page_asset", size=size, asset=asset.key.urlsafe())
        if asset.snippet:
            params['snippet'] = asset.snippet

    else:
        params['src'] = url_for("assets.fallback_asset", size=size, url=quote(entity.url, safe=""))

    return render_template("asset_img.html.j2", **params)


def get_asset_dimensions(asset, size):
    # TODO: Ota huomioon kuvasuhde.
    return ASSET_SIZES[size]


def asset_factory(url, parent, **kwargs):

    app.logger.debug("Creating asset: %s, %s", __name__, url)
    url = url_normalize(url)
    id = u"%s" % uuid.uuid5(uuid.NAMESPACE_URL, url.encode("utf-8"))
    key = ndb.key.Key(Asset, id, parent=parent.key)
    asset = get_by_key(key) or Asset(key=key)

    if key not in model_storage:
        raise UserWarning("Asset %s not in storage!" % repr(key))

    kwargs["url"] = url
    asset.populate(**kwargs)
    return asset


def get_entity_asset(entity):
    asset = None

    for k in model_storage:
        e = model_storage[k]

        if isinstance(e, Asset) and e.key.parent() == entity.key:
            _asset = model_storage[k]
            if not asset or asset.weight > _asset.weight:
                asset = _asset

    if asset:
        # check storage for heavier asset
        _asset = Asset.query(Asset.weight > asset.weight, ancestor=entity.key).order(-Asset.weight).get()
        if _asset:
            asset = _asset
    else:
        asset = Asset.query(ancestor=entity.key).order(-Asset.weight).get()


    return asset


def scrape_asset(asset):
    r = crawler().get(asset.url)

    # Stops if request failed.
    r.raise_for_status()

    i = images.Image(r.content)

    # Select suitable cropping.
    ratio = float(i.width) / float(i.height)

    horizontal = ASSET_SIZES['horizontal'][0] / ASSET_SIZES['horizontal'][1]
    vertical = ASSET_SIZES['vertical'][0] / ASSET_SIZES['vertical'][1]

    size = None

    # TODO: Implement blobstore
    if ratio >= horizontal:
        size = ASSET_SIZES['horizontal']
    elif ratio <= vertical:
        size = ASSET_SIZES['vertical']
    else:
        size = ASSET_SIZES['square']

    size = (200, 200)

    i.resize(*size, crop_to_fit=True)

    asset.width = size[0]
    asset.height = size[1]
    asset.data = i.execute_transforms(output_encoding=images.PNG)

    # Create small snippet image.
    # TODO: Optimize png
    i.resize(10, 10, crop_to_fit=False, allow_stretch=True)
    snippet_data = i.execute_transforms(output_encoding=images.PNG)
    asset.snippet = u"data:image/png;base64,%s" % b64encode(snippet_data)

    return asset


@bp.route('/<size>/<asset>.png')
def page_asset(size, asset):

    if size not in ASSET_SIZES:
        app.logger.debug("Unknown image size requested: %s", repr(size))
        abort(404)

    entity = get_by_key(ndb.key.Key(urlsafe=asset))
    if not entity:
        abort(404)
    elif not isinstance(entity, Asset):
        app.logger.warning(u"Invalid Model type requested")
        abort(404)

    try:
        if not entity.data:
            entity = scrape_asset(entity)
            entity.put()

        resp = make_response(entity.data, 200)
        resp.headers['Cache-Control'] = u"public, max-age=31536000"
        resp.mimetype = "image/png"

        return resp
    except Exception as e:
        app.logger.exception(e)
        # return redirect(entity.url)

@bp.route("/logo/<size>/<url>")
def fallback_asset(size, url):
    """
    Fallback for asset image lookup.

    :param url: Url for original asset.
    """

    url = unquote(url)

    if not valid_url(url):
        abort(404)
    url = url_normalize(url)

    urlparts = urlparse(url)

    return redirect(u"https://logo.clearbit.com/%s" % urlparts.netloc)


