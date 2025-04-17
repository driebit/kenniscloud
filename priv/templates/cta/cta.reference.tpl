{% extends "cta/cta.tpl" %}

{% block cta_class %}cta--reference{% endblock %}

{% block cta_buttons %}
    {% catinclude "page-actions/page-action-add-thing.tpl" id category='reference' new_rsc_title=_'Bron' btn_class="btn--primary" btn_title='Deel een bron' tabs_enabled=['new'] tab='new' public=public %}
{% endblock %}
