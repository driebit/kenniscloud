{% if id.is_editable %}
    <div class="add-suggestion">
        <h4 class="bordered-title"><span>Suggesties bij deze bijdrage:</span></h4>
        {% with id.o.subject|is_a:"library_keyword" as keywords %}
            <div id="{{ #library_suggestions }}" class="library-suggestions">
                {% lazy action={update target=#library_suggestions id=id template="library/suggestions.tpl" keywords=keywords} %}
            </div>
        {% endwith %}

        {% with id.o.dbpedia_suggestion as dbpedia_suggestions %}
            {% include "dbpedia/suggestions.tpl" suggestions=dbpedia_suggestions %}
        {% endwith %}
    </div>
{% endif %}
