{% extends "_ginger_edit.tpl" %}

{% block edit_blocks %}
    <div id="poststuff">
    {% optional include "_translation_init_languages.tpl" %}

    <div class="widget widget--name">
        <div class="widget-content">
            <div class="form-group row">
                <div class="ginger-edit__profile-picture form-group col-lg-2 col-md-2 col-sm-12">
                    <h3 class="section-title">{_ Profile picture _}</h3>
                    <div class="ginger-edit__profile-picture-control">
                        <div id="links-{{ id }}-depiction" data-reload-template="_edit_media.person.tpl">
                            {% include "_edit_media.person.tpl" id=id %}
                        </div>

                        {% if is_editable %}
                            <a class="ginger-edit__profile-picture-button" id="{{ #profile_picture_connect }}" href="#connect-profile-picture" title="{_ Change profile picture _}" aria-label="{_ Change profile picture _}">
                                <i class="icon glyphicon glyphicon-camera" aria-hidden="true"></i>
                            </a>
                            {% wire id=#profile_picture_connect
                                action={dialog_open template="_action_ginger_dialog_connect.tpl"
                                    title=[_"Add:", " ", "afbeelding"]
                                    subject_id=id
                                    edge_template="_rsc_edge_media.tpl"
                                    predicate=`depiction`
                                    cat=`media`
                                    tab="upload"
                                    tabs_enabled=["upload"]
                                    callback=""
                                    actions=[
                                        {postback postback={reload_media id=id rsc_id=id div_id=["links-",id|make_list,"-depiction"]}
                                            delegate="controller_admin_edit"}
                                    ]}
                            %}
                        {% endif %}
                    </div>
                </div>
                <div class="form-group col-lg-4 col-md-4">
                    <label class="control-label" for="name_first">{_ First _}</label>
                    <div>
                        <input class="form-control" id="name_first" type="text" name="name_first" value="{{ id.name_first }}" />
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

    {% if id.category_id.feature_show_address|if_undefined:`true` %}
        {% catinclude "_admin_edit_content_address.tpl" id is_editable=is_editable languages=languages %}
    {% endif %}

    {% if id.is_a.media or id.medium %}
        {% include "_admin_edit_content_media.tpl" %}
    {% endif %}

    </div>
{% endblock %}
