{% extends "_ginger_edit.tpl" %}

{% block edit_blocks %}

    <div id="poststuff">

        {% if q.auteur %}<input type="hidden" name="object|author" value="{{ m.acl.user }}">{% endif %}

        {% if q.kennisgroep %}<input type="hidden" name="content_group_id" value="{{ q.kennisgroep|escape }}">{% endif %}

        <input type="hidden" name="is_published" value="true">

        {% optional include "_translation_init_languages.tpl" %}

        {% catinclude "_ginger_edit_basics.tpl" id is_editable=is_editable languages=languages %}

        {% all catinclude "_admin_edit_content.tpl" id is_editable=is_editable languages=languages %}

        {% if id.category_id.feature_show_address|if_undefined:`true` %}
            {% catinclude "_admin_edit_content_address.tpl" id is_editable=is_editable languages=languages %}
        {% endif %}

        {% catinclude "_admin_edit_body.tpl" id is_editable=is_editable languages=languages %}

        {% if m.acl.is_allowed.delete[id] %}
            {% catinclude "_ginger_edit_depiction.tpl" id is_editable=is_editable languages=languages show_opened tab="upload" tabs_enabled=["upload","video","video_embed"] %}
        {% endif %}

        <div class="c-edit__add-attachement" style="border-radius: 1rem; margin-bottom: 2rem;">
            <p class="u-padding-1"><b>{_ If you want to use the entered address here for the geolocation below, please save the page first. _}</b>
            {% button type="submit" id="save_stay" class="btn btn-default" text=_"Save" title=_"Save this page." disabled=not id.is_editable %}
            </p>

            {% include "_geomap_admin_location.tpl" %}
        </div>
    </div>
{% endblock %}
