{% if id.content_group_id == m.rsc.cg_methodology.id %}
    {% include "global-nav/global-nav-methodology.tpl" %}
{% else %}
    {% catinclude "global-nav/global-nav-kc.tpl" id %}
{% endif %}