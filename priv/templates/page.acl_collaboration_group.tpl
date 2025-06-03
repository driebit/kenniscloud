{% extends "base.tpl" %}

{% block body_class %}t--knowledge-group{% endblock %}

{% block content %}
{% with id as collab_group %}
    {% catinclude "masthead/masthead.tpl" id %}

    <main>
        <div class="kg-intro">
            <div class="kg-intro__content">
                {% spaceless %}
                    {% catinclude "category-of/category-of.tpl" id %}
                    {% include "_hidden-resource.tpl" %}
                {% endspaceless %}

                {% include "page-title/page-title.tpl" id=id %}

                {% include "keywords/status-tags.tpl" id %}

                {% include "summary/summary.tpl" id=id %}

                {% catinclude "keywords/keywords.tpl" id %}

                {% include "page-actions/page-action-edit-thing.tpl" %}
            </div>

            {% catinclude "top-aside/top-aside.tpl" id collab_group=collab_group %}
        </div>
        
        <div class="kg-contributions">
        
            {% include "maplibre/map.tpl" %}

            {% if m.search[{query cat=['contribution', 'event'] content_group=id pagelen=50 }]|timeline_sort:id as results %}
                {% if id.timeline_status == "automatic" %}
                    {% include "_driebit_timeline.tpl" result=results title=_"Timeline of highlights in this knowledge group" titleClass="bordered-title" list_item_template="timeline/_timeline-card.tpl" %}
                {% elseif id.timeline_status == "manual" %}
                    {% include "_driebit_timeline.tpl" result=results|filter:`included_in_timeline`:"1" title=_"Timeline of highlights in this knowledge group" titleClass="bordered-title" list_item_template="timeline/_timeline-card.tpl" %}
                {% endif  %}
            {% endif  %}
        
            {% include "contributions-bar/contributions-bar.tpl" %}

            {% catinclude "main-aside/main-aside.tpl" id %}
        </div>
    </main>
{% endwith %}
{% endblock %}
