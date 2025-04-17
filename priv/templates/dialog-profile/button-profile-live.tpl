{% if m.acl.user as user %}
    {% with #profile as profile_wire_id %}

        <a href="{{ user.page_url }}" id="{{ profile_wire_id }}" class="{{ class }}">
            {% include "avatar/avatar.tpl"
                id=user.id
                fallback_rsc_id=m.rsc.custom_avatar_fallback.id
            %}

            <span>{{ user.title|truncate:20 }}</span>
        </a>

    {% endwith %}
{% endif %}
