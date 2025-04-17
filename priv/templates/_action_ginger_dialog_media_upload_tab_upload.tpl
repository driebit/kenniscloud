{# This will prevent the multiupload module (if enabled) to appear in the frontend edit #}

{% block upload_form %}
    <div class="tab-pane {% if is_active %}active{% endif %}" id="{{ tab }}-upload">
        <p>
            {_ Upload a file from your computer. _}
            {% if not id %}
                {_ You have to specify a description of the file to make it easier to find and share. _}
            {% endif %}
        </p>

        {% wire id=#form type="submit"
            postback={media_upload predicate=predicate actions=actions id=id subject_id=subject_id stay=stay content_group_id=content_group_id callback=callback}
            delegate=`action_admin_dialog_media_upload`
        %}
        <form id="{{ #form }}" method="POST" action="postback" class="form form-horizontal">
            <fieldset>
                {% if not id %}
                    <div class="form-group row">
                        <label class="control-label col-md-3" for="new_media_title">{{ predicate.title }} {_ title _}</label>
                        <div class="col-md-9">
                            <input type="text" class="do_autofocus col-lg-4 col-md-4 form-control" id="new_media_title" name="new_media_title" value="{{ title|escape }}" />
                        </div>
                    </div>
                {% endif %}

                <div class="form-group row">
                    <label class="control-label col-md-3" for="upload_file">{{ predicate.title }} {_ file _}</label>
                    <div class="col-md-9">
                        <input type="file" class="form-control" id="upload_file" name="upload_file" />
                        {% validate id="upload_file" type={presence} %}

                        <input type="checkbox" id="copyright" name="copyright"><label for="copyright" class="control-label">Ik verklaar dat ik de publicatierechten heb op deze foto of afbeelding. Lees <a target="_blank" href="{{ m.rsc.page_image_copyright.page_url }}">hier</a> meer over publicatierechten.</label>
                        {% validate id="copyright" type={presence only_on_submit} %}
                    </div>
                </div>

                <div class="row form-group">
                    <div class="col-xs-12">
                    </div>
                </div>

                <div class="modal-footer">
                    {% button class="btn btn-default" action={dialog_close} text=_"Cancel" tag="a" %}
                    <button class="btn btn-primary" type="submit">{_ Upload file _}</button>
                </div>
            </fieldset>
        </form>
    </div>
{% endblock %}

