<aside>
    {% if id.is_editable %}
        {% include "_ginger_edit_content_add_to_timeline.tpl" %}

        {% include "_ginger_edit_content_status.tpl" %}
        {% catinclude "_ginger_edit_content_status_label.tpl" id %}

        {% include "aside-connection/aside-add-connection.tpl" id=id cat="keyword" predicate="subject" title=_"Keywords" new_rsc_title=_"Keyword" dispatch="ginger_edit" helper_text_top=_"Add keywords so the knowledge can be found easily and relevant connections can be made." %}

        {% include "_ginger_edit_content_publication_date.tpl" %}

        {% include "aside-connection/aside-add-connection.tpl" id=id cat="" predicate="hasreference" title="Achtergrondinformatie" %}
    {% endif %}
</aside>
