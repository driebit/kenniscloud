{% extends "base.tpl" %}

{% block body_class %}-no-bg{% endblock %}

{% block content %}
    {% include "masthead/masthead.text.tpl" %}

    <main id="main-content" class="main-content">
        <div class="roadmap">
            {% include "roadmap/roadmap-actions.tpl" %}

            {% for r in id.o.hasstep %}
                <div class="roadmap__step -{{ r.name }}">
                    <h2 class="roadmap__step__title -{{ r.name }}">{{ r.title }}</h2>

                    <ul class="roadmap__step__option-list">
                        {% for c in r.o.hasoption %}
                            <li>
                                <input type="checkbox" id="option_{{ c.id }}" class="roadmap__step__option-list__option"><label for="option_{{ c.id }}">{{ c.title }}</label>

                                {% if c.body %}
                                    {% button
                                        text=""
                                        class="roadmap__step__option-list__info"
                                        action={dialog_open
                                            title=c.title
                                            template="roadmap/help-dialog.tpl"
                                            id=c.id
                                        }
                                    %}
                                {% endif %}
                            </li>
                        {% endfor %}
                        <li>
                            <input type="text" class="roadmap__step__option-list__other" placeholder="Anders..." value="">
                            <label></label>
                        </li>
                    </ul>
                </div>
            {% endfor %}

            {% include "roadmap/roadmap-actions.tpl" %}
        </div>
    </main>

{% endblock %}