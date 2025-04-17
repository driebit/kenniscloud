{% extends "cta/cta.tpl" %}

{% block cta_class %}cta--event{% endblock %}

{% block cta_buttons %}
    {% catinclude "page-actions/page-action-add-thing.tpl" id category='event' new_rsc_title=_'Meetup' btn_class="btn--primary" btn_title='Organiseer een meetup' tabs_enabled=['new'] tab='new' public=public %}
{% endblock %}
