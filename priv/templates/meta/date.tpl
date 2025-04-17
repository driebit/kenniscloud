
{% if id.date_end|date:"Y":id.date_is_all_day %}
    {{ id.date_start|date:"j":id.date_is_all_day }}
{% else %}
    {{ id.date_start|date:"d M Y":id.date_is_all_day }} {% if not id.date_is_all_day and id.date_start|date:"H:i":id.date_is_all_day!="00:00" %}{{ id.date_start|date:"H:i":id.date_is_all_day }} {% endif %}
{% endif %}

{% if id.date_end|date:"Y":id.date_is_all_day %}
    {% with id.date_start|date:"d-m-Y":id.date_is_all_day != id.date_end|date:"d-m-Y":id.date_is_all_day as show_end_date %}
    {% with id.date_end|date:"H:i":id.date_is_all_day!="00:00" and id.date_end|date:"H:i":id.date_is_all_day!="23:59" and not id.date_is_all_day as show_end_time %}
        {% if show_end_date %}- {% endif %}
        {% if show_end_date %}
            {% if id.date_is_all_day %}{{ id.date_end|date:"j M Y":id.date_is_all_day }}{% else %}{{ id.date_end|date:"j M Y":id.date_is_all_day }}{% endif %}
        {% else %}
            {{ id.date_end|date:"M Y":id.date_is_all_day }}
        {% endif %}
    {% endwith %}
    {% endwith %}
{% endif %}

{% if not id.date_is_all_day and id.date_start|date:"H:i":id.date_is_all_day!="00:00" %}
    <span>
        {{ id.date_start|date:"H:i":id.date_is_all_day }} — {{ id.date_end|date:"H:i":id.date_is_all_day }}
    </span>
{% endif %}
