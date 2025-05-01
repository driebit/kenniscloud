<li class="c-card--feed c-card--feed--event">
    <a href="{{ id.page_url}}" class="c-card--feed__link c-card--feed__link--event">
    </a>
            <a href="{{ id.creator_id.page_url}}">
            {% include "avatar/avatar.tpl" image_class="c-card--feed__avatar" id=id.creator_id fallback_rsc_id=m.rsc.custom_avatar_fallback.id %}
        </a>
    <div class="c-card--feed__background" style="background-image: url('{% image_url id.depiction mediaclass='list-image' crop=dep.crop_center %}');" >
    </div>
    <div class="c-card--feed__text">
        <a href="{{ id.creator_id.page_url}}" class="c-card--feed__author-link">
            <strong>{{ id.creator_id.title }}</strong> 
        </a>
        {_ heeft een meetup aangemaakt _}
        {% if id.o.hasregion as region %}
            {_ in de regio _} <a href="{{ region.page_url }}" class="c-card--feed__author-link"><strong>{{ region.title }}</strong></a>,
        {% endif %}

        {% if id.content_group_id as kg %}
            {_ in de kennisgroep _} <a href="{{ kg.page_url }}" class="c-card--feed__author-link"><strong>{{ kg.title }}</strong></a>
        {% endif %}

        <strong><p>
        {{ id.title }} 
        | {{ id.date_start|date:"j.n.Y" }} 
        {% if id.date_end %}
        - {{ id.date_end|date:"j.n.Y" }}
        {% endif %}
        </p></strong>

    </div>
    <span class="c-card--feed__time">{{ now|timesince:id.created:"":1 }}</span>
</li>