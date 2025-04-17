{% extends "email_base.tpl" %}

{% block title %}Reacties op "{{ topic }}"{% endblock %}

{% block body %}
    <p>Beste {{ m.rsc[user_id].name_first }},</p>

    {% for paragraph in content %}
        <p>{{ paragraph }}</p>
    {% endfor %}

    <p>
        Klik op onderstaande link om de reacties te bekijken:<br/>
        <a href="{{ link }}">{{ link }}</a>
    </p>
{% endblock %}

{% block footer %}
    <tr>
        <td>
            <p>
            Je ontvangt maximaal twee mails per dag om je te informeren over voor jouw relevante toevoegingen aan KennisCloud. Dit kunnen reacties zijn op bijdragen die jij bent gestart of waaraan je hebt deelgenomen, of waarin iemand jou getagd heeft
            Wil je deze notificaties niet meer per mail ontvangen, <a href="{{ unsubscribe_url }}">klik dan hier om je uit te schrijven</a>.
            </p>
        </td>
    </tr>
{% endblock %}
