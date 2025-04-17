{% with m.rsc[id.content_group_id] as group %}

    {% if group.is_a.acl_collaboration_group %}
        <p class="part-of">
            <span>{_ Part of: _}</span>
            <p>
                <a href="{{ group.page_url }}" class="">{{ group.title }}</a><br/>
            </p>
        </p>
    {% endif %}

{% endwith %}
