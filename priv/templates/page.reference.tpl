{% extends "page.event.tpl" %}

{% block page_actions %}
	{% include "page-actions/page-action-like.tpl" %}
{% endblock %}

{% block og_data %}
    {% with id.o.hasbanner|default:id.o.depiction.title as dep_title %}
        {% if dep_title /= id.og_title %}
            {% include "og/og-data.tpl" %}
        {% endif %}
    {% endwith %}
{% endblock %}

{% block page_meta %}
	<div class="page-meta">
        {% include "meta/remarks.tpl" %}
		{% include "meta/likes.tpl" %}
    {% include "page-actions/page-action-flag.tpl" %}
		{# {% include "meta/meta-website.tpl" %} #}
	</div>
{% endblock %}

{% block page_body %}
    <div class="page-body">
	   {{ id.body }}
    </div>
{% endblock %}
