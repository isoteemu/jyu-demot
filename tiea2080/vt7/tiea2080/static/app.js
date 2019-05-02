$(document).ready(function() {
    $('#nav-notification').popover({
        content: $('#_notifications').html(),
        html: true,
    }).click(function(e) {
        e.preventDefault();
    });

    if($('#_notifications .alert-danger').length) {
        $('#nav-notification .material-icons').text('notification_important');
    } else if($('#_notifications .alert-primary').length) {
        $('#nav-notification .material-icons').text('notifications');
    }

    // Make popovers dismissable.
    $('.popover-dismiss').popover({
        trigger: 'focus'
    });
});
