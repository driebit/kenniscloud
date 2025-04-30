<aside>

    {% if id.is_editable %}

        {% include "_driebit_edit_aside_acl_collaboration_group.tpl" %}

        {% include "aside-connection/aside-add-connection.tpl" id=id cat="image" predicate="hasbanner" %}

        {% include "aside-connection/aside-add-connection.tpl" id=id cat="keyword" predicate="subject" %}

        {% include "aside-connection/aside-add-connection.tpl" id=id cat="person" predicate="hascollabmember" title=_'Lid' tabs_enabled=["find"] tab="find" %}

        {% include "aside-connection/aside-add-connection.tpl" id=id cat="person" predicate="hascollabmanager" title=_'Beheerder' tabs_enabled=["find"] tab="find" %}

        {% include "aside-connection/aside-add-connection.tpl" id=id cat="acl_collaboration_group" predicate="has_subgroup" title=_'Subgroup' tabs_enabled=["find"] tab="find" %}
        
    {% endif %}

</aside>
