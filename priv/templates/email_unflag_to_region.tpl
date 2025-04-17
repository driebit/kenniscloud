{% extends "email_base.tpl" %}

{% block title %}Markering als ongepast ingetrokken{% endblock %}

{% block body %}
<p>
Een markering van de volgende bijdrage is ingetrokken:
<br/>
<a href="{{ contribution.page_url_abs }}">{{ contribution.page_url_abs }}</a>
</p>
<p>
{% if flags > 2 %}
   NB: Er staan nog {{ flags }} markeringen voor deze bijdrage geregistreerd.
{% elif flags == 1 %}
   NB: Er staat nog een andere markering voor deze bijdrage geregistreerd.
{% else %}
   Er staan geen andere markeringen meer voor deze bijdrage geregistreerd.
{% endif %}
</p>
{% endblock %}
