Tree = function(element) {
    this.$element = $(element);
    this.$list = $('ul', element);
    this.init();
};

Tree.prototype = new function() {
    this.init = function() {
        var _this = this;
        this.$list.click(function(e) {
            var $target = $(e.target);
            if ($target.hasClass('icon')) {
                var $li = $target.closest('li');
                _this.toggle($li);
            }
        });

        var children = $('.level_1', this.$element);
        for (var i=0, l = children.length; i < l; i++) {
            toggleVis.call(this, $(children[i]), false);
        }
    };

    this.toggle = function($li) {
        var closed = !$li.hasClass('closed'),
            children = $($li[0]).nextUntil('.level_0');
        $li.toggleClass('closed');
        for (var i=0, l = children.length; i < l; i++) {
            toggleVis.call(this, $(children[i]), !closed);
        }
    };

    function toggleVis($li, show) {
        $li.css('display', show ? '' : 'none');
    }
};
