{% extends "email_base.tpl" %}

{% block title %}Welkom bij "{{ group_id.title }}"{% endblock %}

{% block body %}
    <p>Beste {{ user_id.name_first|default:user_id.title }},</p>
    <p>Welkom bij {{ group_id.title }}!</p>
    <p>{{ message|unescape|escape_link }}</p>

    <p>
        Klik op onderstaande link om naar het Kennisgroep te gaan:<br/>
        <a href="{{ group_id.page_url_abs }}">{{ group_id.page_url_abs }}</a>
    </p>
{% endblock %}

{% block footer %}
{% endblock %}
