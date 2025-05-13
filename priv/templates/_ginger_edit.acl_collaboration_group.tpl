{% extends "_ginger_edit.tpl" %}

{% block edit_blocks %}
    <div id="poststuff">
    {% optional include "_translation_init_languages.tpl" %}
    {% catinclude "_ginger_edit_basics.tpl" id is_editable=is_editable languages=languages %}

    {% all catinclude "_admin_edit_content.tpl" id is_editable=is_editable languages=languages %}

    {% if id.category_id.feature_show_address|if_undefined:`true` %}
        {% catinclude "_admin_edit_content_address.tpl" id is_editable=is_editable languages=languages %}
    {% endif %}

    {% if id.is_a.media or id.medium %}
        {% include "_admin_edit_content_media.tpl" %}
    {% endif %}

    {% catinclude "_admin_edit_body.tpl" id is_editable=is_editable languages=languages %}
    {% catinclude "_ginger_edit_depiction.tpl" id is_editable=is_editable languages=languages show_opened tab="upload" tabs_enabled=["upload","video","video_embed"] %}
    </div>
{% endblock %}