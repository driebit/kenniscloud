{% extends "_ginger_edit.tpl" %}

{% block edit_blocks %}

    <div id="poststuff">
        {% if q.auteur %}<input type="hidden" name="object|author" value="{{ m.acl.user }}">{% endif %}

        {% if q.kennisgroep %}<input type="hidden" name="content_group_id" value="{{ q.kennisgroep|escape }}">{% endif %}

        <input type="hidden" name="is_published" value="true">

	    {% catinclude "_ginger_edit_basics_form.tpl" id is_editable=is_editable languages=languages %}

        <div class="frontend-edit-text">
            <label class="control-label">Tekst</label>
            <p class="control-label-summary">Vervolg hier je verhaal</p>
            {% catinclude "_admin_edit_body.tpl" id is_editable=is_editable languages=languages %}
        </div>

        {% catinclude "_ginger_edit_depiction.tpl" id languages=languages show_opened tab="upload" tabs_enabled=["upload"] %}

        <div class="form-group clearfix c-edit__add-document">
            <div class="widget-header">{_ Gekoppelde documenten _}</div>
            <div id="links-{{ id }}-document" data-reload-template="_edit_media.tpl">
                {% block allowed_types %}
                {% endblock %}

                <div id="{{ #unlink_document_message }}"></div>
                {% live template="_edit_media_document_list.tpl"
                        id=id
                        unlink_message=#unlink_document_message
                        topic={object id=id predicate=`hasdocument`}
                %}

            </div>


            <div class="form-group clearfix c-document-upload">
                <div class="pull-left">

                    {% if required %}
                        <input type="hidden" id="{{ #document }}" value="0" />
                        {% validate id=#document type={presence failure_message=_"Field is required"} type={custom against="window.has_connection" failure_message=_"Field is required" args=`links-`++id++`-document` } only_on_submit %}
                    {% endif %}

                    <a class="btn btn-default btn-add-thing btn-large" id="{{ #connect }}" href="#connect">{_ + Upload a document _}</a>
                    {% wire id=#connect
                        action={dialog_open
                            template="_action_dialog_connect.tpl"
                            intent="connect"
                            title=_"Upload document (PDF)"
                            subject_id=id
                            page=page
                            edge_template="_rsc_edge_document.tpl"
                            cat=m.rsc.media.id
                            predicate=`hasdocument`
                            tab="upload"
                            tabs_enabled=["find", "upload"]
                            accept="application/pdf"
                            }
                    %}
                </div>
            </div>
        </div>

        <div class="c-edit__add-attachement" style="border-radius: 1rem; margin-bottom: 2rem;">
            {% include "_admin_edit_content_address_contribution.tpl" class="test" %}
            <p class="u-padding-1"><b>Als u het ingevoerde adres hier wilt gebruiken voor de geolocatie hieronder, sla dan eerst de pagina op. </b>
            {% button type="submit" id="save_stay" class="btn btn-default" text=_"Save" title=_"Save this page." disabled=not id.is_editable %}
            </p>

            {% include "_geomap_admin_location.tpl" %}
        </div>

    </div>
{% endblock %}
