{% with sidebar_id|default:"map__sidebar" as sidebar_id %}
	<button id="infobox__close" class="infobox__close"><i class="icon--close"></i></button>
	<ul class="infobox__results">
        {% for qid in q.ids|default:[q.id] %}
            {% if qid %}
                {% if infobox_template %}
                    {% include infobox_template qid=qid %}
                {% else %}
                    {% catinclude "list/list-item-map.tpl" qid %}
                {% endif %}
            {% endif %}
        {% endfor %}
	</ul>

{% wire id="infobox__close" action={remove_class target=sidebar_id class="is-active"} %}

{% endwith %}
