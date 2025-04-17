<aside class="main-aside">
    {% if id.query %}
        {% with m.search[{query query_id=id pagelen=12 page=q.page}] as result %}
            {% include "list/list.tpl" list_id="list--query" class="list--kg" list_template="list/list-item-kg.tpl" hide_showall_button items=result extraClasses="" id=id %}
        {% endwith %}
    {% endif %}
</aside>
