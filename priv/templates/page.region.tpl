{% extends "base.tpl" %}

{% block body_class %}t--region{% endblock %}

{% block content %}

    {% catinclude "masthead/masthead.tpl" id %}
    {% with m.search[{query hasobject=[id,'hasregion'] cat="acl_collaboration_group" pagelen=1000}] as kc_groups %}
        <main>
            <div class="kg-intro">
                <div class="kg-intro__content">
                    {% catinclude "category-of/category-of.tpl" id %}

                    {% include "page-title/page-title.tpl" id=id %}

                    {% include "summary/summary.tpl" id=id %}

                    {% catinclude "keywords/keywords.tpl" id %}

                    {% include "page-actions/page-action-edit-thing.tpl" %}
                </div>

                {% catinclude "top-aside/top-aside.tpl" id kc_groups=kc_groups %}
            </div>

            <div class="kg-contributions">
                {% catinclude "main-aside/main-aside.tpl" id kc_groups=kc_groups %}
            </div>

            {# {% catinclude "cta/cta.tpl" id %} #}
        </main>

    {% endwith %}

{% endblock %}
