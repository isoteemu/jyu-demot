function since(datetime) {
    var since_values = {
        "m": 60,
        "h": 60*60,
        "d": 60*60*24,
        "m": 60*60*24*30.4,
        "y": 60*60*24*30*12+5,
    };

    var lapsed = (Date.now() - datetime.valueOf()) / 1000;

    var r = lapsed+"s";
    for(var k in since_values) {

        m = since_values[k];
        d = lapsed / m;
        if( d > 1 )
            r = Math.round(d) + k;
    }
    return r;
}

$(document).ready(function() {
    $('#nav-notification').popover({
        content: $('#_notifications').html(),
        html: true,
    }).click(function(e) {
        e.preventDefault();
    });

    /* Currently this doesnt' work, as notifications are stored on <template> tag */
    if($('#_notifications .alert-danger').length) {
        $('#nav-notification .material-icons').text('notification_important');
    } else if($('#_notifications .alert-primary').length) {
        $('#nav-notification .material-icons').text('notifications');
    }

    // Make popovers dismissable.
    $('.popover-dismiss').popover({
        trigger: 'focus'
    });

    $('#article-modal').on('show.bs.modal', function (event) {
        var start_slide = 0;
        var $article = $(event.relatedTarget).parents(".article");
        var $feed = $(".feed [name=feed][value=\""+$article.find("[data-feed]").data("feed")+"\"]");
        var $articles = $article.parents(".articles").find(".article");
        var $modal = $(this);
        var $header = $modal.find("header");

        console.log($article.find("data-feed"), $feed);
        $header.find(".feed").empty().append($feed.html());
        $header.find("#modal-title-title").attr("href", $article.find("a").attr("href")).empty().text($article.find(".title").text());

        published = new Date($article.find("[data-published]").data("published"));
        $header.find("#modal-title-published").attr("datetime", published.toISOString()).empty().text(since(published));

        // BODY
        var $indicators = $modal.find(".carousel-indicators");
        var $carousel = $modal.find(".carousel-inner");

        var $indicator_tmpl = $modal.find("template#carousel-indicator").html().trim();
        var $item_tmpl = $modal.find("template#carousel-item").html().trim();

        $indicators.empty();
        $carousel.empty();

        for(var i=0; i < $articles.length; i++) {
            var $article_i = $($articles[i]);

            var $large_url = $article_i.find("img[data-large]").data("large");

            var $slide = $($item_tmpl);
            var $indicator = $($indicator_tmpl);

            $indicator.attr("data-slide-to", i);

            $slide.find("img").attr("src", $large_url);
            $slide.find(".carousel-caption").html(
                DOMPurify.sanitize($article_i.find(".content").html(), {
                    ALLOWED_TAGS: ['b', 'strong', 'em', 'p', 'a', 'br', 'tt']
                })
            );

            $slide.attr("title", $article_i.find(".title").text())
            $slide.attr("href", $article_i.find("a").attr("href"));

            if($article_i[0] == $article[0]) {
                start_slide = i;
                $slide.addClass("active");
                $indicator.addClass("active");
            }

            $indicators.append($indicator);
            $carousel.append($slide);
        }

        $carousel.parents(".carousel").carousel(start_slide).on('slide.bs.carousel', function(e) {
            // Update title
            $header.find("#modal-title-title").attr("href", $(e.relatedTarget).attr("href"))
                .empty().text($(e.relatedTarget).attr("title"));
        });

    });
});
