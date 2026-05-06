{# Same as for any other 'collection', but sorting by most recently contributed to: #}
{% if id.o.haspart %}
    <aside class="main-aside">
        {% with m.search.paged[{query cat="acl_collaboration_group" hassubject=[id,'haspart'] is_published="true" sort="-pivot.kenniscloud_groups.last_contributed" pagelen=6}] as result %}

            {% include "list/list.tpl" list_id="list--haspart" items=result extraClasses="" id=id noresults=" " %}

        {% endwith %}
    </aside>
{% endif %}
