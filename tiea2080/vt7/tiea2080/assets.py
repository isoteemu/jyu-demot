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

from base64 import b64encode
import os
from urllib import quote, unquote
import uuid

from . import memcache, csrf
from .utils import url_normalize, valid_url, urlparse, crawler
from .models import Asset, ndb, get_by_key, model_storage

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
        'alt': Markup(entity.title).striptags(),
    }

    asset = get_entity_asset(entity)

    # TODO: Recurse into parents.
    if asset:
        if asset.blob_key:
            size_px = ASSET_SIZES[size]
            size_longest = max(size_px)

            crop = True if size_px[0] == size_px[1] else False
            secure = request.is_secure

            img_url = images.get_serving_url(blob_key=asset.blob_key, size=size_longest, crop=crop, secure_url=secure)

            params['src'] = img_url

        else:

            db_asset = asset.key.get()

            if db_asset and asset.blob_key is not False:
                try:
                    asset_key = asset.key.urlsafe()
                    taskqueue.add(
                        url=url_for('assets.scrape_asset_task'),
                        params={'asset': asset_key},
                        method='GET',
                        name="asset-scrape-%s" % asset_key
                    )
                except (taskqueue.TombstonedTaskError, taskqueue.TaskAlreadyExistsError) as e:
                    app.logger.info(u"Asset scraping task recently scheduled for url: %s (key: %s)", asset.url, asset_key)
                    pass

            params['src'] = asset.url

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
    r"""
    Download image, and store cropped version.

    """

    app.logger.debug("Scraping image %s", repr(asset.url))

    r = crawler().get(asset.url)

    # Stops if request failed.
    r.raise_for_status()

    i = images.Image(r.content)

    # Select suitable cropping.
    ratio = float(i.width) / float(i.height)

    horizontal = ASSET_SIZES['horizontal'][0] / ASSET_SIZES['horizontal'][1]
    vertical = ASSET_SIZES['vertical'][0] / ASSET_SIZES['vertical'][1]

    size = None

    if ratio >= horizontal:
        size = ASSET_SIZES['horizontal']
    elif ratio <= vertical:
        size = ASSET_SIZES['vertical']
    else:
        size = ASSET_SIZES['square']

    i.resize(*size, crop_to_fit=True)

    asset.width = size[0]
    asset.height = size[1]

    img_data = i.execute_transforms(output_encoding=images.JPEG)
    key = send_into_blobstore(img_data, asset.key.urlsafe())
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
            entity = scrape_asset(entity)
            entity.put()
            return "Added: %s" % repr(entity.blob_key), 202
        else:
            return "Asset already in storage", 200

    except Exception as e:
        app.logger.exception(e)
        # return redirect(entity.url)
        abort(500)


@bp.route("/asset/<size>/<url>")
def fallback_asset(size, url):
    """
    Fallback for asset image lookup.

    :param url: Url for original asset.
    """

    url = unquote(url)

    if not valid_url(url):
        abort(404)

    size_px = max(ASSET_SIZES.get(size, (200,200)))

    url = url_normalize(url)
    urlparts = urlparse(url)

    return redirect(u"https://logo.clearbit.com/%s?size=%d" % (urlparts.netloc, size_px))


