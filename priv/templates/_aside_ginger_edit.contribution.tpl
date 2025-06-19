<aside>
    {% if id.is_editable %}
        {% include "_ginger_edit_content_add_to_timeline.tpl" %}

        {% include "_ginger_edit_content_status.tpl" %}
        {% catinclude "_ginger_edit_content_status_label.tpl" id %}

        {% include "aside-connection/aside-add-connection.tpl" id=id cat="keyword" predicate="subject" dispatch="ginger_edit" helper_text_top="Voeg tags toe zodat de kennis goed vindbaar wordt en er relevante verbindingen gemaakt kunnen worden." %}

        {% include "_ginger_edit_content_publication_date.tpl" %}
    {% endif %}
</aside>
