{% extends "_ginger_edit.tpl" %}

{% block form_save %}
    {# Hidden safe buttons and publish state - controlled via the nabvar #}
    <div style="display: none">
        <span id="button-prompt">{% block nav_prompt %}{{ id.category_id.title }}{% endblock %}</span>
adfadfasdf
        {% block buttons %}
            {% button type="submit" id="save_stay" class="btn btn-primary" text=_"Save" title=_"Save this page." disabled=not id.is_editable %}

            {% if id.is_editable %}
                {% button type="submit" id="save_view" class="btn btn-default" text=_"Save &amp; view" title=_"Save and view the page." %}
            {% else %}
                {% button id="save_view" class="btn btn-primary" text=_"View" title=_"View this page." action={redirect id=id} %}
            {% endif %}
        {% endblock %}
    </div>
{% endblock %}

{% block edit_blocks %}
    <div id="poststuff">
        {% if id.is_editable %}
            {% include "aside-connection/aside-add-connection.tpl" id=id cat="step" predicate="hasstep" dispatch="ginger_edit" title='Stappen' %}
        {% endif %}
    </div>
{% endblock %}

