
(function ($) {
    'use strict';

    $.widget('ui.filter_form', {
        _create: function () {
            var me = this,
                widgetElement = $(me.element);

            me.widgetElement = widgetElement;

            $('.members__filter__location').on('click', me.doFilter);
            $('.members__filter__keyword').on('click', me.doFilter);
        },

        doFilter: function() {
            console.log("click");
        }

    })
})(jQuery);
