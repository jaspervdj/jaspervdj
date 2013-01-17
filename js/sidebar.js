$(document).scroll(function() {
    var left = $(document).scrollLeft();
    $('#navigation').css('left', left > 0 ? -left + 'px' : '0px');
});
