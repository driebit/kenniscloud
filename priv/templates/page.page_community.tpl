{% extends "page.collection.tpl" %}
{% block page_filters %}
<div class="page-filters">
    {% catinclude "filters/community-filters.tpl" id %}
</div>
{% endblock %}
