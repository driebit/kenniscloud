{% if m.search[{query match_objects=id is_published is_findable cat_exclude=['media', 'person', 'event', 'acl_collaboration_group', 'keyword'] pagelen=6}] as result %}

    <h3 class="bordered-title">Ook interessant</h3>
    {% include "list/list.tpl" list_id="list--match-objects" id=id class="list--kg" list_template="list/list-item-kg.tpl" items=result extraClasses="" noresults=" " %}

{% endif %}
