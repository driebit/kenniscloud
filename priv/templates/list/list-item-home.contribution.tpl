<li class="c-card--feed">
    <a href="{{ id.page_url}}" class="c-card--feed__link">

        <a href="{{ id.creator_id.page_url}}">
            {% include "avatar/avatar.tpl" image_class="c-card--feed__avatar" id=id.creator_id fallback_rsc_id=m.rsc.custom_avatar_fallback.id %}
        </a>

        <div class="c-card--feed__text">
            <a href="{{ id.creator_id.page_url}}" class="c-card--feed__author-link">
                <strong>{{ id.creator_id.title }}</strong> 
            </a>
            {_ heeft een bijdrage gemaakt in de kennisgroep _} <a href="{{ kg.page_url }}" class="c-card--feed__author-link"><strong>{{ id.content_group_id.title }}</strong></a>
            : {{ id.body|trim|truncate:160}}
        </div>
        <span class="c-card--feed__time">{{ now|timesince:id.created:"":1 }}</span>
    </a>
</li>