{% with
    direction|default:"out",
    max|default:'none'
as
    direction,
    max
%}

    {% if id.o.about|is_undefined or id.o.about|length < 1 %}
        <div id="elm-references"></div>
        {% javascript %}
            var referencesElement = document.getElementById("elm-references");
            var app = Elm.AddReference.init({
                node: referencesElement,
                flags: {{ id }}
            });
        {% endjavascript %}
    {% endif %}

    {% include "_ginger_connection_widget.tpl" predicate_ids=[m.rsc.about.id] direction=direction %}

{% endwith %}
