{% extends "global-nav/global-nav-kc.tpl" %}

{% block global_nav_extra_classes %}-methodology{% endblock %}

{% block logo %}
    <a href="{{ m.rsc.cg_methodology.page_url }}" class="global-nav__logo">
        <img src="/lib/images/cloud.svg" alt=""> KennisCloud <span class="-methodology"> Methodiek </span>
    </a>
{% endblock %}

{% block menu %}
    <ul class="global-nav__menu -methodology">
        {% for r in m.rsc.cg_methodology.o.subnavigation %}
            <li><a href="{{ r.page_url }}">{{ r.title }}</a></li>
        {% endfor %}
    </ul>
{% endblock %}

{% block actions %}
    {% include "toggle-menu/toggle-menu.tpl" %}
{% endblock %}
