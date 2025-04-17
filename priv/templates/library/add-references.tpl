{% if id.is_editable %}
    <div id="add-references" class="add-references is-open">
        <h3 class="add-references__title" data-content="content" data-parent="add-references">Voeg relevante achtergrondinformatie toe aan je bijdrage {% button class="add-references__button" action={toggle_class target="add-references" class="is-open"} action={toggle_class target="content" class="u-d-hidden"} text='<i class="icon--arrow-down"></i>' %}</h3>

        <div id="content" class="add-references__content">
            {% include "library/add-source.tpl" %}
        </div>
    </div>
{% endif %}
