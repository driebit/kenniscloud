{% include "_admin_edit_floating_buttons.tpl" %}

<div id="sort"> {# also sidebar #}
    {% include "_admin_edit_content_publish.tpl" headline="simple" %}

    {% if id.is_a.meta %}
    {% include "_admin_edit_meta_features.tpl" %}
    {% endif %}

    {% include "_admin_edit_content_acl.tpl" %}

    {% if not id.is_a.meta %}
    {% include "_admin_edit_content_pub_period.tpl" %}
    {% include "_admin_edit_content_date_range.tpl" show_header %}
    {% endif %}

    {% if m.crowdparticipant[id].has_data %}
    {% include "_admin_edit_sidebar_crowdparticipant.tpl" id=id %}
    {% endif %}

    {% all catinclude "_admin_edit_sidebar.tpl" id languages=languages %}

    {% include "_admin_edit_content_page_connections.tpl" %}
</div>
