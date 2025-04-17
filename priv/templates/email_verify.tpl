{% extends "email_base.tpl" %}

{% block title %}{_ Please confirm your account _}{% endblock %}

{% block depiction %}
    <tr>
        <td style="background-color: #ffffff;">
            <a href="{{ m.site.protocol }}://{{ m.site.hostname }}/">
                <img src="{% image_url 'lib/images/logo.png' upscale width=314 height=44 crop absolute_url %}" width="600" height="" alt="KennisCloud" border="0" style="width: 100%; max-width: 15rem; height: auto; background: #dddddd; font-family: sans-serif; font-size: 15px; line-height: 15px; color: #555555; margin: auto; display: block;" class="g-img">
            </a>
        </td>
    </tr>
{% endblock %}

{% block body %}
<p>{_ Dear _} {% include "_name.tpl" id=user_id %},</p>

<p>{_ Thank you for registering at our site. We request you to confirm your account before you can use it. _}</p>

<p>{_ Please follow the link below. _}</p>

<p><a href="{% url signup_confirm key=verify_key absolute_url %}">{_ Confirm my account. _}</a></p>

<p>{_ If the link does not work then you can go to _} <a href="{% url signup_confirm key=verify_key absolute_url %}">{% url signup_confirm key=verify_key absolute_url %}</a>. {_ Hope to see you soon. _}</p>

<p>{_ Kind regards, _}</p>
<p><a href="{{ m.site.protocol }}://{{ m.site.hostname }}/">KennisCloud</a></p>
{% endblock %}
