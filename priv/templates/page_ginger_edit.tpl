{% overrules %}

{% block header %}
    {# The buttons in the navbar click/sync with hidden counter parts in the resource edit form #}
    <header class="ginger-edit__header">
        <nav class="ginger-edit__nav navbar navbar-savebuttons">
            {% if id|is_a:"roadmap" %}
                {% button class="btn ginger-edit-save btn--save" text=_"View" title=_"View"
                        action={script script="$('#save_view').click();"}
                %}
            {% else %}
                {% button class="btn ginger-edit-save btn--save" text=_"Save" title=_"Save"
                        action={script script="$('#save_view').click();"}
                %}
            {% endif %}

            {% if zotonic_dispatch == 'contribution_edit' %}
                {# Make sure to delete temporary contributions right away, as
                ## some actions in the edit page can change its version (e.g
                ## change to task), making this resource survive the task to
                ## cleanup temporary rscs #}
                {% button
                    class="btn--cancel"
                    text=_"Delete"
                    action={delete_rsc id=id on_success={redirect back}}
                    tag="a"
                %}
            {% else %}
                {% button class="btn--cancel" text=_"Cancel" action={redirect back} tag="a" %}
            {% endif %}
        </nav>
        {% block header_text %}
            <h1 class="page-title">{_ Edit _}: {% if id.name_first %} {{ id.name_first }} {% if id.name_middle %} {{ id.name_middle }} {% endif %}{% if id.name_surname %} {{ id.name_surname }}{% endif %}{% elif id.title %} {{ id.title }}{% else %}{{ id.category.id.title }}{% endif %}</h1>
        {% endblock %}
    </header>
{% endblock %}

{% block footer %}
    <nav class="ginger-edit__nav--footer">
        <div class="navbar-inner row">
            <div class="col-xs-12" id="save-buttons">

                {% if (id.category_id != m.rsc.person.id) and (not id.is_protected and m.rsc[id].is_deletable) %}
                    {% button class="btn--delete" id="delete-button" text=_"Delete" action={dialog_delete_rsc id=id.id on_success={redirect back}} title=_"Delete this page." %}
                {% endif %}


                {% if id|is_a:"roadmap" %}
                    {% button class="btn--save" text=_"View" title=_"view"
                            action={script script="$('#save_view').click();"}
                    %}
                {% else %}
                    {% button class="btn--save" text=_"Save" title=_"Save"
                            action={script script="$('#save_view').click();"}
                    %}
                 {% endif %}
            </div>
        </div>
    </nav>

    {% include "_ginger-editor.tpl" overrides_tpl="_ginger_edit_tinymce_overrides_js.tpl" %}
{% endblock %}
