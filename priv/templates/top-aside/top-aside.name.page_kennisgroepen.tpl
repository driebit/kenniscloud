<p>Een vraagstuk waar jij, je organisatie of omgeving niet uitkomt? Samen kom je verder!</p>

{% if not m.acl.user %}
    <a href="{% url logon p={page id=id}|url %}" class="btn--primary">Meld je aan</a>
{% else %}
    <a href="mailto:{{ m.rsc.acl_user_group_community_librarian.email }}?subject=Kennisgroep starten" class="btn--primary">Start een nieuwe kennisgroep </a>
{% endif %}

