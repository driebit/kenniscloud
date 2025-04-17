{% extends "email_base.tpl" %}

{% block title %}Bijdrage gemarkeerd als ongepast{% endblock %}

{% block body %}
<p>
De volgende bijdrage is gemarkeerd als ongepast:
<br/>
<a href="{{ contribution.page_url_abs }}">{{ contribution.page_url_abs }}</a>
</p>
<p>
{% if flags > 1 %}
   Er staan in totaal {{ flags }} markeringen voor deze bijdrage geregistreerd.
{% else %}
   Dit is op het moment de enige melding.
{% endif %}
</p>
{% endblock %}
