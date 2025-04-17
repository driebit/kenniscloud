<aside>

    {% if id.is_editable %}

        <b>Om aanmelden mogelijk te maken, moet zowel de begin als de einddatum ingevuld worden. Dit mag ook dezelfde dag zijn.</b>
        {% include "_ginger_edit_content_date_range.tpl" show_header is_editable %}
        {% include "_ginger_edit_content_publication_date.tpl" %}
        
        <div class="form-group">
        <input type="hidden" id="is_subjects_checked" name="is_subjects_checked" value="1">
        {% validate id="is_subjects_checked" type={postback event="validate_subjects" id=id} only_on_submit %}
        {% include "aside-connection/aside-add-connection.tpl" id=id cat="keyword" predicate="subject" dispatch="ginger_edit" helper_text_top="Voeg minimaal 4 tot maximaal 17 thema-tags toe, zodat deelnemers aan de meetup hun interesses aan kunnen geven." %}
        </div>
        {% include "aside-connection/aside-add-connection.tpl" id=id cat="person" predicate="author" dispatch="ginger_edit" preset_id=q.auteur|escape title="Auteur" %}

        {% include "aside-connection/aside-add-connection.tpl" id=id cat="image" predicate="hasbanner" tab="upload" tabs_enabled=["upload"] title=_'Banner' dispatch="ginger_edit" %}

    {% endif %}

</aside>
