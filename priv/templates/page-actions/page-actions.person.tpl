{% include "page-actions/page-action-edit-thing.tpl" btn_label="Bewerk profiel" %}

{% if m.acl.user == id %}
<a href="{% url logoff %}" class="page-action--logoff">{{ logoff_label|if_undefined:_"Sign out" }} <i class="#"></i> </a>
{% endif %}
