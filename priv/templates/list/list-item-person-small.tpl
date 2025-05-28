{% with collab_group|default:(m.kc_collab_group.collab_group_of[context_rsc]) as collab_group %}
{% with
    class|default:"person",
    project|default:(collab_group.s.has_subgroup|first),
    m.kc_user[id].is_community_librarian,
    m.kc_collab_group[collab_group|default:project].roles_of[id]
as
    class,
    project,
    is_librarian,
    roles
%}
    {% with m.kc_user[id].specialist_predicates_for[project] as active_specialist_predicates %}
        {% with
            ((is_librarian and ((`manager`|member:roles) or not collab_group))|if:
                "person--cl":
            ((`project_leader`|member:roles)|if:
                "person--pl":
            ((`manager`|member:roles)|if:
                "person--manager":
            (active_specialist_predicates|if:
                "person--specialist":
                ""))))
        as
            badge_class
        %}
            {% if nolink == "true" or not id.is_published %}
                <div class="{{ class }} do_touch_hover {{ badge_class }}">
            {% else %}
                <a href="{{ id.page_url }}" class="{{ class }} do_touch_hover {{ badge_class }}">
            {% endif %}
                {% if not id.is_published %}
                    {% include "avatar/avatar.tpl" id=m.rsc.custom_avatar_fallback.id fallback_rsc_id=m.rsc.custom_avatar_fallback.id %}
                {% else %}
                    {% include "avatar/avatar.tpl" id=id fallback_rsc_id=m.rsc.custom_avatar_fallback.id %}
                {% endif %}
                <div class="person__content">
                    {% if not id.is_published %}
                        <strong>{{ id.title }}</strong>
                    {% else %}
                        <strong>{% include "person/person-title.tpl" id=id %}</strong>
                        {% if is_librarian %}
                            <small>Community Librarian</small>
                        {% elseif (`project_leader`|member:roles) %}
                            <small>Projectleider</small>
                        {% elseif (`manager`|member:roles) %}
                            <small>Beheerder</small>
                        {% elseif active_specialist_predicates %}
                            <small>{{ active_specialist_predicates[1].title }}</small>
                        {% endif %}
                        {% if id.subtitle %}
                            <small>{{ id.subtitle|truncate:40 }}</small>
                        {% endif %}
                        {% if summary and id.summary %}
                            <p>{{ id.summary|truncate:50 }}</p>
                        {% endif %}
                    {% endif %}
                </div>
            {% if nolink == "true" or not id.is_published %}
                </div>
            {% else %}
                </a>
            {% endif %}
        {% endwith %}
    {% endwith %}
{% endwith %}
{% endwith %}
