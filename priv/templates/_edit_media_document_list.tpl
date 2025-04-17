<ul id="{{ #list }}" class="media list-unstyled">
    {% for object_id, edge_id in m.edge.o[id].hasdocument %}
        {% include "_rsc_edge_document.tpl" subject_id=id unlink_message=unlink_message %}
    {% endfor %}
</ul>
{% sorter id=#list
    tag={object_sorter predicate=`hasdocument` id=id}
    placeholder="ui-sortable-placeholder"
    delegate=`controller_admin_edit`
%}