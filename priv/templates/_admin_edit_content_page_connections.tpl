{% overrules %}
{# Overruled to have a more limited set of tabs enabled for the 'hasextra_faq' and 'hasextra_rsc' predicate #}

{% block widget_content %}

<div id="unlink-undo-message"></div>

{% with predicate_ids|default:id.predicates_edit as pred_shown %}
    {% for name, p in m.predicate %}
        {% if p.id|member:pred_shown and not p.id.is_connections_hide %}
            {% if name == "hasextra_faq" or name == "hasextra_rsc" %}
                <h4>{{ p.title }}</h4>

                {% live template="_admin_edit_content_page_connections_list.tpl"
                    topic={object id=id predicate=name}
                    id=id
                    predicate=name|as_atom
                    button_label=button_label
                    button_class=button_class
                    dialog_title_add=dialog_title_add
                    callback=callback
                    action=action
                    nocatselect=nocatselect
                    content_group=content_group
                    unlink_action=unlink_action
                    undo_message_id="unlink-undo-message"
                    list_id=list_id
                    tabs_enabled=["new"]
                %}

                <hr>
            {% elseif name == 'hasextra_img' %}
                <h4>{{ p.title }}</h4>

                {% live template="_admin_edit_content_page_connections_list.tpl"
                    topic={object id=id predicate=name}
                    id=id
                    predicate=name|as_atom
                    button_label=button_label
                    button_class=button_class
                    dialog_title_add=dialog_title_add
                    callback=callback
                    action=action
                    nocatselect=nocatselect
                    content_group=content_group
                    unlink_action=unlink_action
                    undo_message_id="unlink-undo-message"
                    list_id=list_id
                    tabs_enabled=["find", "new"]
                %}

                <hr>
            {% elseif name == 'hasextra_doc' %}
                <h4>{{ p.title }}</h4>

                {% live template="_admin_edit_content_page_connections_list.tpl"
                    topic={object id=id predicate=name}
                    id=id
                    predicate=name|as_atom
                    button_label=button_label
                    button_class=button_class
                    dialog_title_add=dialog_title_add
                    callback=callback
                    action=action
                    nocatselect=nocatselect
                    content_group=content_group
                    unlink_action=unlink_action
                    undo_message_id="unlink-undo-message"
                    list_id=list_id
                    tabs_enabled=["find", "upload", "new"]
                %}

                <hr>
            {% elif name != "depiction" %}
                <h4>{{ p.title }}</h4>

                {% live template="_admin_edit_content_page_connections_list.tpl"
                    topic={object id=id predicate=name}
                    id=id
                    predicate=name|as_atom
                    button_label=button_label
                    button_class=button_class
                    dialog_title_add=dialog_title_add
                    callback=callback
                    action=action
                    nocatselect=nocatselect
                    content_group=content_group
                    unlink_action=unlink_action
                    undo_message_id="unlink-undo-message"
                    list_id=list_id
                    tabs_enabled=tabs_enabled
                %}

                <hr>
            {% endif %}
        {% endif %}
    {% endfor %}
{% endwith %}

{% if not hide_all_connections %}
    <div class="form-group">
       <a class="btn btn-default btn-sm" href="{% url admin_edges qhassubject=id %}"><i class="glyphicon glyphicon-list"></i> {_ View all connections _}</a>
    </div>
{% endif %}

{% endblock %}
