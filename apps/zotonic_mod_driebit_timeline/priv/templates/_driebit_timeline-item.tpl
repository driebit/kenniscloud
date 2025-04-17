<a href={{ id.page_url }} class="c-timeline__link">
<div class="c-timeline__item">
    <div class="c-timeline__dot"></div>
    <div class="c-timeline-card">
        <span class="c-timeline__date">{{ id.publication_start|date:"d M y" }}</span>

        {% if id.depiction|is_visible %}
            {% image id.depiction class="c-timeline-card__image" %}
        {% endif %}

        <h3 class="c-timeline-card-title">{{ id.title }}</h3>
        <p class="c-timeline__location">{{ id.o.location }}</p>
        <p class="c-timeline__excerpt">{{ id.summary }}</p>
    </div>
</div>
</a>