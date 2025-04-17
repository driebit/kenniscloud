<aside>

    {% if id.is_editable %}
        {% include "aside-connection/aside-add-connection.tpl" id=id cat="keyword" predicate="subject" dispatch="ginger_edit" helper_text_top="Voeg tags toe zodat de kennis goed vindbaar wordt en er relevante verbindingen gemaakt kunnen worden." %}

        {% if m.rsc.located_in %}
            {% include "aside-connection/aside-add-connection.tpl" id=id cat="location" predicate="located_in" title=_'Location' %}
        {% endif %}

        {% include "aside-connection/aside-add-connection.tpl" id=id cat="reference" predicate="about" dispatch="ginger_edit" title='Over' tabs_enabled=['find'] tab='find' %}

        {# include "_admin_edit_content_date_range.tpl" show_header is_editable #}
    {% endif %}
</aside>
