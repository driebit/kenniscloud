{% with
        "No remarks",
        "1 reactie",
        "reacties"
    as
        none,
        singular,
        plural
%}

    {% if count == 0 %}
        {{ none }}
    {% elseif count == 1 %}
        {{ singular }}
    {% else %}
        {{ count }} {{ plural }}
    {% endif %}

{% endwith %}
