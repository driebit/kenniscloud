{% with title|default:_"About:" as title %}
{% with m.remarks[id].topic as topic %}
{% with (id.o.about|exclude:`name`) as about %}
{% with topic|if:((about -- [topic]) ++ [topic]):about as about_with_topic %}
    {% if about_with_topic %}
        <div class="part-of--aside">
            <h3 class="part-of__title">{{ title }}</h3>
            <ul>
                {% for r in about_with_topic %}
                    {% include "list/list-item.tpl" id=r %}
                {% endfor %}
            </ul>
        </div>
    {% endif %}
{% endwith %}
{% endwith %}
{% endwith %}
{% endwith %}
