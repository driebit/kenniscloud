<aside>

    {% if id.is_editable %}
        {% include "aside-connection/aside-add-connection.tpl" id=id cat="keyword" predicate="subject" %}
        {% include "aside-connection/aside-add-connection.tpl" id=id cat="person" predicate="author" title=_'Author' %}
        {% include "aside-connection/aside-add-connection.tpl" id=id cat="" predicate="haspart" %}
    {% endif %}

</aside>
