# -*- coding: utf-8 -*-

r"""
Asset handling.

TODO: Use blobstore

"""
from flask import (Blueprint, abort, current_app as app, request,
                   render_template, url_for, redirect, Markup, jsonify)
from werkzeug import parse_options_header

from google.appengine.api import images
from google.appengine.api import blobstore
from google.appengine.api import taskqueue

from requests.exceptions import RequestException

from base64 import b64encode
import os
from datetime import datetime, timedelta

from . import memcache, csrf
from .utils import url_normalize, valid_url, urlparse, crawler, requests
from .models import (
    Asset,
    AssetedModel,
    ndb,
    get_by_key,
)

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

    app.register_blueprint(bp)

    app.jinja_env.filters.update(
        asset_img=asset_img
    )


def asset_img(entity, size=None):
    r"""
    Jinja filter for displaying asset image

    ..usage:
        >>> {{ entity|asset_img }}
        >>> {{ entity|asset_img('<ASSET_SIZE>') }}

    """

    if isinstance(entity, ndb.Key):
        entity = get_by_key(entity)

    if not entity:
        app.logger.warning(u"Can't create image; Entity is missing.")
        return Markup(u'<img src="data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7" class="missing-entity" /><!-- Entity missing -->')

    if size is None:
        size = entity._get_kind()

    if size not in ASSET_SIZES:
        raise ValueError("Asset size ``%s`` not found for asset." % size)

    width, height = get_asset_dimensions(entity, size)
    params = {
        'src': "data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7",
        'width': width,
        'height': height,
        'alt': Markup(entity.title).striptags(),
        'aspect': 1,
        'large': ""
    }

    asset = get_entity_asset(entity)

    if asset:
        # If blob url is already known, hot link to it. If not,
        # set into redirection url.

        params['src'] = bloburl(asset, size,
                                url_for("assets.redirect_to_asset", size=size, asset=asset.key.urlsafe()),
                                fast=True)

        params['large'] = asset.url

        if asset.width and asset.height:
            large_size = get_large_size(asset.width, asset.height)
            params['large'] = bloburl(asset, large_size, url_for("assets.redirect_to_asset", size=large_size, asset=asset.key.urlsafe()), fast=True)
            params['aspect'] = float(asset.width) / float(asset.height)

        params['snippet'] = asset.snippet
    else:
        params['src'] = asset_img_fallback(entity, (width, height))

    return render_template("asset_img.html.j2", **params)


def bloburl(entity, size, fallback=None, fast=False):
    r"""
    Retrieves serving url for Asset.

    :param fast: Don't try creating asset serving url. Only return
                 from memcache.

    """
    if not isinstance(entity, Asset):
        raise TypeError("Expected `Asset` as entity type.")

    if not entity.blob_key:
        return fallback

    size_px = ASSET_SIZES[size]
    size_longest = max(size_px)

    # TODO: Tarkista järkevyys.
    crop = True if size_px[0] == size_px[1] else False
    secure = request.is_secure

    # Look serving url from memcache. Create one if not found.
    key = u"blobstore_asset:%s:%s" % (entity.blob_key, size)
    img_url = memcache.get(key)
    if not img_url and not fast:
        img_url = images.get_serving_url(blob_key=entity.blob_key, size=size_longest, crop=crop, secure_url=secure)
        memcache.set(key, img_url)

    return img_url if img_url else fallback


def asset_img_fallback(asset, size=(200, 200)):

    if isinstance(asset, Asset):
        asset = get_by_key(asset.key.parent())

    size_longest = max(size)
    url = url_normalize(asset.url)
    urlparts = urlparse(url)

    return u"https://logo.clearbit.com/%s?size=%d" % (urlparts.netloc, size_longest)


@bp.route("/asset/<size>/<asset>")
def redirect_to_asset(size, asset):
    if size not in ASSET_SIZES:
        app.logger.debug("Requested unknown size: %s", repr(size))
        abort(404)

    key = ndb.key.Key(urlsafe=asset)
    entity = get_by_key(key)

    if not isinstance(entity, Asset):
        abort(404)

    url = url_normalize(entity.url)

    if entity.blob_key:
        try:
            img_url = bloburl(entity, size)
            # Get google provided asset, and redirect to it.
            response = redirect(img_url, code=301)
            response.headers['Cache-Control'] = "public, max-age=31536000"
            return response
        except Exception as e:
            # TODO: Handle ObjectNotFoundError.
            app.logger.info(u"Asset blob %s was not found: %s", entity.blob_key, e)
            entity.blob_key = None
    elif url:
        # Check that entity is really in datastore, and not just stub from memcache.
        db_asset = entity.key.get()

        if db_asset and entity.blob_key is None:
            # Schedule asset scraping. Until it has been done, redirect into associated image.
            try:
                asset_key = entity.key.urlsafe()
                taskqueue.add(
                    url=url_for('assets.scrape_asset_task'),
                    params={'asset': asset_key},
                    method='GET',
                    name="asset-scrape-%s" % asset_key
                )
            except (taskqueue.TombstonedTaskError, taskqueue.TaskAlreadyExistsError):
                app.logger.debug(u"Asset scraping task recently scheduled for url: %s (key: %s)", entity.url, asset_key)
                pass

        return redirect(entity.url)
    else:
        # Redirect into entity url
        return redirect(asset_img_fallback(entity, ASSET_SIZES[size]))

    app.logger.warning(u"This should not happen; Nothing found for suitable asset resource")
    abort(404)


def get_asset_dimensions(asset, size):
    # TODO: Ota huomioon kuvasuhde.
    return ASSET_SIZES[size]


def get_large_size(width, height):
    r"""
    Returns best match for large size image.
    """

    horizontal_ratio = ASSET_SIZES['horizontal'][0] / ASSET_SIZES['horizontal'][1]
    vertical_ratio = ASSET_SIZES['vertical'][0] / ASSET_SIZES['vertical'][1]

    # Select suitable cropping.

    ratio = float(width) / float(height)
    if ratio >= horizontal_ratio:
        size = "horizontal"
    elif ratio <= vertical_ratio:
        size = "vertical"
    else:
        size = "square"

    return size


def asset_factory(url, parent, **kwargs):

    app.logger.debug("Creating asset: %s for %s", url, repr(parent.key.id()))
    url = url_normalize(url)
    id = u"%s" % url

    key = ndb.key.Key(Asset, id, parent=parent.key)
    asset = get_by_key(key) or Asset(key=key)

    kwargs["url"] = url
    asset.populate(**kwargs)

    parent.tell_about_asset(asset)

    return asset


def get_entity_asset(entity):
    if not isinstance(entity, AssetedModel):
        raise TypeError("`entity` needs to be AssetedModel")

    asset = entity.Asset()
    return asset


def scrape_asset(asset):
    r"""
    Download image, and store cropped version.
    """

    app.logger.debug("Scraping image %s", repr(asset.url))

    asset.updated = datetime.utcnow()

    response = crawler().get(asset.url)

    response.raise_for_status()

    i = images.Image(response.content)

    size_key = get_large_size(i.width, i.height)
    size = ASSET_SIZES[size_key]

    i.resize(*size, crop_to_fit=True)

    asset.width = size[0]
    asset.height = size[1]

    img_data = i.execute_transforms(output_encoding=images.JPEG)
    key = send_into_blobstore(img_data, u"%s" % response.url)
    asset.blob_key = key

    # Create small snippet image.
    # TODO: Optimize png
    i.resize(10, 10, crop_to_fit=False, allow_stretch=True)
    snippet_data = i.execute_transforms(output_encoding=images.PNG)
    asset.snippet = u"data:image/png;base64,%s" % b64encode(snippet_data)

    return asset


def send_into_blobstore(img_data, filename):
    r"""
    Send data into blobstore.

    We need to create our own http request for it.
    """
    handler_url = url_for('assets.send_into_blobstore_handler')
    post_url = blobstore.create_upload_url(handler_url)

    r = crawler().post(post_url, files={'file': (filename, img_data)})
    r.raise_for_status()

    json = r.json()

    app.logger.info("Submitted asset into filestore: %s", repr(json))
    return json['blob_key']


@bp.route('/task/store-into-blobstore', methods=('POST',))
@csrf.exempt
def send_into_blobstore_handler():

    blob = request.files.values()[0]

    blob_key = parse_options_header(blob.headers['Content-Type'])[1]['blob-key']

    return jsonify({
        'status': 'OK',
        'filename': blob.filename,
        'blob_key': blob_key
    })


@bp.route("/task/scrape-asset")
def scrape_asset_task():

    asset = request.args.get("asset")

    # Look datastore only for asset
    entity = ndb.key.Key(urlsafe=asset).get()
    if not entity:
        app.logger.warning(u"Missing entity:")
        return "Asset not in datastore"
    elif not isinstance(entity, Asset):
        app.logger.warning(u"Model type not `Asset`: %s", type(entity))
        abort(500)

    try:

        if not entity.blob_key:
            try:
                entity = scrape_asset(entity)
                entity.put()
                return "Added: %s" % repr(entity.blob_key), 202
            except RequestException as e:
                app.logger.exception(e)

                # If asset is too old (1 week), and we haven't managed to dowload
                # it yet, give up and remove it.
                now = datetime.utcnow()
                max_age = entity.created + timedelta(weeks=1)
                if now > max_age:
                    app.logger.warning("Removing week old asset as dead.")
                    entity.delete()
                    return "Asset removed as dead", 410
                else:
                    # Every time entity scrape fails, lower its' weight by 10%
                    entity.weight = int(max(0, entity.weight * 0.9))
                    entity.put()
            except images.NotImageError as e:
                # Delete if asset was found, but is not image.
                app.logger.warning("Asset %s is not image: %s", repr(entity.url), unicode(e))
                entity.delete()
                return "Asset was not image.", 410

        else:
            return "Asset already in storage", 200

    except Exception as e:
        app.logger.exception(e)
        # return redirect(entity.url)
        abort(500)
