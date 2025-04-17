{% with btn_class|default:"btn--primary -icon" as btn_class %}
{% if id|member:m.acl.user.s.hascollabmember or id|member:m.acl.user.s.hascollabmanager or id|member:m.acl.user.hascollabmanager %}
    {% block cta_buttons %}
        <a href="{% url contribution_edit kennisgroep=id.content_group_id auteur=m.acl.user %}" class="{{ btn_class }}">Plaats een bijdrage</a>

        <a href="{% url meetup_edit kennisgroep=id.content_group_id auteur=m.acl.user %}" class="{{ btn_class }}">Organiseer een meetup</a>
    {% endblock %}
{% else %}
    {% include "page-actions/page-action-join-group.tpl" id=id.content_group_id %}
{% endif %}
{% endwith %}
