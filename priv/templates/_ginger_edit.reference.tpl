{% extends "_ginger_edit.tpl" %}

{% block edit_blocks %}
    <div id="poststuff">
        {% if q.auteur %}<input type="hidden" name="object|author" value="{{ m.acl.user }}">{% endif %}
        {% if q.kennisgroep %}<input type="hidden" name="content_group_id" value="{{ q.kennisgroep|escape }}">{% endif %}

        <input type="hidden" name="is_published" value="true">

        {% catinclude "_ginger_edit_basics_form.tpl" id is_editable=is_editable languages=languages %}

        {% include "_ginger_edit_website.tpl" id=id %}

    	<h3>Tekst</h3>
        {% catinclude "_admin_edit_body.tpl" id is_editable=is_editable languages=languages %}

    </div>
{% endblock %}
