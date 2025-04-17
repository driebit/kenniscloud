{% if id.o.hasreference|reversed as references %}
    <div class="page-aside__reference">
        <h3 class="bordered-title">Toegevoegde achtergrondinformatie</h3>
        <ul>
            {% for reference in references %}
                {% catinclude "list/list-item-small.tpl" reference r=reference %}
            {% endfor %}
        </ul>
    </div>
{% endif %}
