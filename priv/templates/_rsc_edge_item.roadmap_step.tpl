{#
Link contents of connection list item.

Params:
- id
#}
{% with id.title as title %}
{% image id mediaclass="admin-list-dashboard" %}
<span class="menu-label">{% if title %}{{ title }}{% else %}<em>{_ untitled _}</em>{% endif %}</span>
{% endwith %}
