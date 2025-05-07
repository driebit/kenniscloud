<a href={{ id.page_url }} class="c-timeline__link">
    <div class="c-timeline__item">
        <div class="c-timeline__dot"></div>
        <div class="c-timeline-card">
            <div class="u-d-flex u-flex-justify-between">
                {% block card_time_category %}
                    <span class="c-timeline__date">{{ id.publication_start|date:"j F Y" }}</span>
                    <span class="c-timeline__category">{{ id.category.name }}</span>
                {% endblock %}
            </div>
            <hr class="c-timeline__separator"></hr>

            {% block card_content %}
                {% if id.depiction|is_visible %}
                    {% image id.depiction class="c-timeline-card__image" %}
                {% endif %}

                <h3 class="c-timeline-card-title">{{ id.title }}</h3>
                <p class="c-timeline__excerpt">{{ id.summary|truncate:100 }}</p>
            {% endblock %}
        </div>
    </div>
</a>