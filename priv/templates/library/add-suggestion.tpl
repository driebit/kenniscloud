{% if id.is_editable %}
    <div class="add-suggestion">
        <h4 class="bordered-title"><span>Suggesties bij deze bijdrage:</span></h4>
            <div id="{{ #library_suggestions }}" class="library-suggestions">
                {% lazy action={update target=#library_suggestions id=id template="library/suggestions.tpl"} %}
            </div>
    </div>
{% endif %}
