{% with id.o.subject|is_a:"library_keyword" as keywords %}
    {% if m.library.suggestions[keywords]|slice:[,5] as suggestions %}
        <ul id="library-suggestions-list">
            {% for suggestion in suggestions %}
                {% include "list/list-item-small-library.tpl" r=suggestion %}
            {% endfor %}
        </ul>
    {% elif id.o.subject %}
        <p><a href="{% url ginger_edit id=id %}">Koppel andere tags</a> aan deze bijdrage, dan heb je meer kans op suggesties uit de bibliotheekcatalogus.</p>
    {% else %}
        <p><a href="{% url ginger_edit id=id %}">Voeg tags toe</a> aan deze bijdrage om suggesties uit de bibliotheekcollectie te krijgen.</p>
    {% endif %}
{% endwith %}
