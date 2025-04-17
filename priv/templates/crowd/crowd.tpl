{% extends "base.tpl" %}

{% block body_class %}t--crowd{% endblock %}
{% block global_nav %}{% if not m.crowd[id].is_daycrowd %}{% catinclude "global-nav/global-nav.tpl" id %}{% endif %}{% endblock %}
{% block content %}

    <div class="page-header crowd-page-header">
        <div class="page-intro page-intro--event crowd-page-intro">
            <div class="category-of">
	            <span class="category-of__cat">
			        <span>{{ m.rsc[id.category.id].title }}</span>
                    <a href="/page/{{ id }}">{{ title|default:id.title }}</a>
	            </span>
	            {% if rsc_id %}
		        <a href="{{ m.rsc[rsc_id].page_url }}" class="category-of__kg">{{ m.rsc[rsc_id].title }}</a>
	            {% endif %}
	            {% if id.address_city %}
    	        <i class="icon--location"></i> {{ id.address_city|truncate:35 }}
	            {% endif %}
            </div>
            <h1 class="page-title">Meetup Crowd</h1>
        </div>
    </div>

    <div id="elm-crowd"></div>
    {% wire name="register_crowd_participant" postback={register crowd=id} delegate="mod_crowdparticipant" %}
    {% javascript %}
    var crowdElement = document.getElementById("elm-crowd");
    var app = Elm.Crowd.init({
        node: crowdElement,
        flags: {"url": location.href,
                "host": location.origin,
                "editableInMeetupMode": {{m.acl.is_allowed.update[id]}},
                "mode": "{{ id.category.name }}",
                "justRegistered": {{ m.crowdparticipant.participates_in[id] }}
                }
    });
    app.ports.register.subscribe(function(data) { z_event('register_crowd_participant', data); });

    cotonic.broker.subscribe("bridge/origin/model/edge/event/{{ id }}/s/+", function(topic, msg) {
        app.ports.onTopic.send({"topic": topic, "msg": msg});
    });
    {% endjavascript %}

{% endblock %}

{% block footer %}{% if not m.crowd[id].is_daycrowd %}{% include "footer/footer.tpl" %}{% endif %}{% endblock %}
