{% include "_admin_edit_floating_buttons.tpl" %}

<div id="sort"> {# also sidebar #}
    {% include "_admin_edit_content_publish.tpl" headline="simple" %}

    {% include "_admin_edit_content_acl.tpl" %}

    {% include "_admin_edit_content_pub_period.tpl" %}
    {% include "_admin_edit_content_date_range.tpl" show_header %}

    {% include "_admin_edit_sidebar_crowdlink.tpl" %}

    {% all catinclude "_admin_edit_sidebar.tpl" id languages=languages %}

    {% include "_admin_edit_content_page_connections.tpl" %}
</div>
