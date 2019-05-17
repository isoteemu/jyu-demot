function since(datetime) {
    var since_values = [
        ["s", 1],
        ["m", 60],
        ["h", 60*60],
        ["d", 60*60*24],
        ["m", 60*60*24*30.4],
        ["y", 60*60*24*30.4*12],
    ];

    var lapsed = (Date.now() - datetime.valueOf()) / 1000;

    var r = lapsed+"s";
    for(var i of since_values) {
        var k = i[0];
        var m = i[1];
        d = lapsed / m;

        if( d > 1 )
            r = Math.round(d) + k;

    }
    return r;
}

$(window).scroll(function() {
    var top = $(document).scrollTop();
    // There is small dead zone on purpose, to eliminate possible flickering.
    if (top > 20) {
        $("#site-header").addClass("shrink");
    } else if(top < 40) {
        $("#site-header").removeClass("shrink");
    }
});

$(".navbar .dropdown").hover(function() {
    $(this).find("[role=\"button\"]").addClass("show");
    $(this).find(".dropdown-menu").addClass("show");
}, function() {
    $(this).find("[role=\"button\"]").removeClass("show");
    $(this).find(".dropdown-menu").removeClass("show");
});

$(document).ready(function() {

    $('#nav-notification').popover({
        content: $('#_notifications').html(),
        html: true,
        sanitize: false,
    }).click(function(e) {
        e.preventDefault();
    });

    /* Currently this doesnt' work, as notifications are stored on <template> tag */
    if($('#_notifications .alert-danger').length) {
        $('#nav-notification .material-icons').text('notification_important');
    } else if($('#_notifications .alert-primary').length) {
        $('#nav-notification .material-icons').text('notifications');
    }

    /* Search field reset button. Reset value and set focus. */
    $(".search-box input[type=reset]").click(function(e) {
        $search = $("#search-input");
        $search.val("");
        $search.focus();
    });

    // Make popovers dismissable.
    $('.popover-dismiss').popover({
        trigger: 'focus'
    });

    // Buttons with confirm
    $('button.btn-warning[data-confirm]').click(function() {
        return confirm($(this).data("confirm"));
    });
    
    function carousel_pre_activate(e) {
        var $slide = $(e);
        if(e.relatedTarget) 
            $slide = $(e.relatedTarget);

        $("#article-modal #modal-title-title").attr("href", $slide.attr("href"))
            .empty().text($slide.attr("title"));

        $("#article-modal header .feed").empty().append($slide[0].feed.html());

        published = new Date($slide[0].article.find("[data-published]").data("published"));
        $("#article-modal #modal-title-published").attr("datetime", published.toISOString()).empty().text(since(published));
    }

    function carousel_post_activate(e) {
        var $slide = $(e);
        if(e.relatedTarget) 
            $slide = $(e.relatedTarget);

        // TODO: Resizing is not required, but inject something.
        // Resize image to desired size.
        // $img = $slide.find("img").first();
        // if(! $img.attr("src")) {
        //     // If we have no img, set size to caption contents size.
        //     var size = 0;
        //     $slide.find(".carousel-caption").children().each(function() {
        //         size += $(this).outerHeight();
        //     });
        //     size += $slide.find(".carousel-caption").outerHeight() + 15;
        //     $img.css("height", size+"px");
        // }
    }

    $('#article-modal').on('show.bs.modal', function (event) {
        var start_slide = 0;
        var $article = $(event.relatedTarget).parents(".article");
        var $feed = $(".feed [name=feed][value=\""+$article.find("[data-feed]").data("feed")+"\"]");
        var $articles = $article.parents(".articles").find(".article");
        var $modal = $(this);
        var $header = $modal.find("header");

        // BODY
        var $indicators = $modal.find(".carousel-indicators");
        var $carousel = $modal.find(".carousel-inner");
        $carousel.parents(".carousel").carousel("dispose");

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

            $slide.find(".carousel-caption").html(
                DOMPurify.sanitize($article_i.find(".content").html(), {
                    ALLOWED_TAGS: ['b', 'strong', 'em', 'p', 'a', 'br', 'tt', 'var']
                })
            );

            $slide.find("img").attr("src", $large_url);
            $slide.attr("title", $article_i.find(".title").text())
            $slide.attr("href", $article_i.find("a").attr("href"));
            $slide[0].feed = $(".feed [name=feed][value=\""+$article_i.find("[data-feed]").data("feed")+"\"]");
            $slide[0].article = $article_i;

            // Article save button.
            $save_form = $slide.find("form.save-btn");
            $save_form[0].article = $article_i;

            // Set target
            $save_form.attr("action", $save_form.attr("action")+encodeURIComponent($article_i.find("[data-article]").data("article")));
            $save_form.submit(function(e) {
                $form = $(e.target);
                e.preventDefault();

                $.ajax($form.attr("action"), settings={
                    data: $form.serializeArray(),
                    method: $form.attr("method"),
                    success: function(data) {
                        // Toggegle save icon
                        if(data['status'] == "OK") {
                            if($form.attr("method") == "POST") {
                                $form.attr("method", "DELETE");
                                $form.find(".material-icons").text("bookmark");
                                $form[0].article.find("[data-saved]").data("saved", (new Date()).toISOString());
                            } else {
                                $form.attr("method", "POST");
                                $form.find(".material-icons").text("bookmark_border");
                                $form[0].article.find("[data-saved]").data("saved", "");
                            }
                        }
                    }
                });
            });

            // Set save icon.
            if($article_i.find("[data-saved]").data("saved")) {
                $save_form.find(".material-icons").text("bookmark");
                $save_form.attr("method", "DELETE");   
            }

            // Active slide
            if($article_i[0] == $article[0]) {
                start_slide = i;
                $slide.addClass("active");
                $indicator.addClass("active");
                carousel_pre_activate($slide);
                carousel_post_activate($slide);
            }

            $indicators.append($indicator);
            $carousel.append($slide);
        }

        $carousel.parents(".carousel").carousel({
            interval: 20000,
            keyboard: true,
        }).on('slide.bs.carousel', carousel_pre_activate).on('slid.bs.carousel', carousel_post_activate);
        $carousel.parents(".carousel").carousel(start_slide).carousel("pause");

    });
});
