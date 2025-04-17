{% extends "page.tpl" %}
{% block body_class_category %} {% endblock %}
{% block body_class %}t--person{% endblock %}

{% block title %}Bevestiging uitschrijven mail{% endblock%}

{% block content %}
<main>
    <div class="page-container">
        <div class="page-body">
            {% if m.acl.user as user %}
                <h3>Bevestiging</h3>
                {% if q.mailing == "weekly" %}
                    {% wire postback={unsubscribe mailing="weekly"} delegate=`kenniscloud` %}
                    <p>{{ user.email }} is uitgeschreven voor wekelijkse updates.</p>
                {% else %}
                    {% wire postback={unsubscribe mailing="notifications"} delegate=`kenniscloud` %}
                    <p>{{ user.email }} is uitgeschreven voor notificatie-mails.</p>
                {% endif %}
                <p>Op <a href="{{ user.page_url }}">je profielpagina</a> kan je je weer inschrijven.</p>
            {% else %}
                {% wire action={redirect} %}
            {% endif %}
        </div>
    </div>
</main>
{% endblock %}
