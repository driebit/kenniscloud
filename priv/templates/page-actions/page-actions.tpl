<div class="page-actions">
    {# {% catinclude "share/share.tpl" id %} #}

    {% catinclude "page-actions/page-action-like.tpl" id %}

    {% block more_page_actions %}
    {% endblock %}

    {% catinclude "page-actions/page-action-edit-thing.tpl" id %}
</div>
