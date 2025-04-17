{% extends "_ginger_edit.tpl" %}

{% block edit_blocks %}
    <div id="poststuff">
    {% optional include "_translation_init_languages.tpl" %}
    <div class="widget widget--name">
        <div class="widget-content">
            <div class="form-group row">
                <div class="form-group col-lg-4 col-md-4">
                    <label class="control-label" for="name_first">{_ First _}</label>
                    <div>
                        <input class="form-control" id="name_first" type="text" name="name_first" value="{{ id.name_first }}" />
                    </div>
                </div>
                <div class="form-group col-lg-2 col-md-2">
                    <label class="control-label" for="name_middle">{_ Middle _}</label>
                    <div>
                        <input class="form-control" id="name_middle" type="text" name="name_middle" value="{{ id.name_middle }}" />
                    </div>
                </div>
                <div class="form-group col-lg-2 col-md-2">
                    <label class="control-label" for="name_surname_prefix">{_ Sur. prefix _}</label>
                    <div>
                        <input class="form-control" id="name_surname_prefix" type="text" name="name_surname_prefix" value="{{ id.name_surname_prefix }}" />
                    </div>
                </div>
                <div class="form-group col-lg-4 col-md-4">
                    <label class="control-label" for="name_surname">{_ Surname _}</label>
                    <div>
                        <input class="form-control" id="name_surname" type="text" name="name_surname" value="{{ id.name_surname }}" />
                    </div>
                </div>
            </div>
        </div>
    </div>

    {% catinclude "_ginger_edit_basics.tpl" id is_editable=is_editable languages=languages %}

    {# {% all catinclude "_admin_edit_content.tpl" id is_editable=is_editable languages=languages %} #}

    {% if id.category_id.feature_show_address|if_undefined:`true` %}
        {% catinclude "_admin_edit_content_address.tpl" id is_editable=is_editable languages=languages %}
    {% endif %}

    {% if id.is_a.media or id.medium %}
        {% include "_admin_edit_content_media.tpl" %}
    {% endif %}

    {# {% catinclude "_admin_edit_body.tpl" id is_editable=is_editable languages=languages %} #}

    {# catinclude "_admin_edit_blocks.tpl" id is_editable=is_editable languages=languages #}

    <div class="ginger-edit__profile-picture">
        <h3 class="section-title">{_ Profile picture _}</h3>
        {% catinclude "_ginger_edit_depiction.tpl" id is_editable=is_editable languages=languages widget_title=_" " show_opened tab="upload" tabs_enabled=["upload"] %}
    </div>

    {# {% catinclude "_ginger_edit_depiction.tpl" id is_editable=is_editable languages=languages show_opened tab="upload" tabs_enabled=["upload","video","video_embed"] %} #}
    </div>
{% endblock %}
