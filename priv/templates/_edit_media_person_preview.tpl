{% with m.edge.o[id].depiction as depictions %}
    <div class="ginger-edit__profile-picture-frame {% if not depictions %}is-empty{% endif %}">
        {% if depictions %}
            {% for object_id, edge_id in depictions %}
                {% if forloop.last %}
                    {% image object_id mediaclass="profile_avatar" alt=id.title class="ginger-edit__profile-picture-image" %}
                {% endif %}
            {% endfor %}
        {% else %}
            <i class="glyphicon glyphicon-user" aria-hidden="true"></i>
        {% endif %}
    </div>
{% endwith %}
