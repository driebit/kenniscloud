{% with (id.category.name == "remark")|if:("#"++id):"" as query %}
<li class="c-card--feed">
    <a href="{{ id.o.about.page_url}}{{query}}" class="c-card--feed__link">

        <a href="{{ id.creator_id.page_url}}">
            {% include "avatar/avatar.tpl" image_class="c-card--feed__avatar" id=id.creator_id fallback_rsc_id=m.rsc.custom_avatar_fallback.id %}
        </a>

        <div class="c-card--feed__text">
            <a href="{{ id.creator_id.page_url}}" class="c-card--feed__author-link">
                <strong>{{ id.creator_id.title }}</strong> 
            </a>
            reageerde op het bijdrage van 
            <a href="{{ id.o.about.creator_id.page_url}}" class="c-card--feed__author-link">
                <strong>{{ id.o.about.creator_id.title }}</strong>
            </a>
            : {{ id.body|truncate:180}}
        </div>
        <span class="c-card--feed__time">{{ now|timesince:id.created:"":1 }}</span>
    </a>
</li>
{% endwith %}