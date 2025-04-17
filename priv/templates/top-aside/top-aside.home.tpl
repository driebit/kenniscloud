{% if m.acl.user %}
<aside class="c-homepage__aside">
    <h3 class="c-homepage__aside-title">Je bent aangemeld voor:</h3>
    <p class="c-homepage__aside-section-title"><i class="icon--location"></i>Regio</p>
    <ul class="c-homepage__aside-list">

        {% if m.acl.user.o.hasregion as regions %}

            {% if regions|length <= 3 %}
                {% for region in regions %}
                    <li><a href="{{ region.page_url }}" class="c-homepage__aside-link">{{ region.title }}</a></li>
                {% endfor %}
            {% else %}
                {% for region in regions|slice:[1, 3] %}
                    <li><a href="{{ region.page_url }}" class="c-homepage__aside-link">{{ region.title }}</a></li>
                {% endfor %}
                <div id="show-all-regions" style="display: none;">
                    {% for region in regions|slice:[4,] %}
                        <li><a href="{{ region.page_url }}" class="c-homepage__aside-link">{{ region.title }}</a></li>
                    {% endfor %}
                </div>
                {% include "show-hide-button.tpl" button_id="show-hide-regions-btn" %}
                {% wire
                    id="show-hide-regions-btn"
                    action={toggle target="show-all-regions"}
                %}
            {% endif %}

        {% else %}
            <li class="c-homepage__aside-link">Geen regio's</li>

        {% endif %}
        
    </ul>
    <p class="c-homepage__aside-section-title"><i class="icon--ob-arrow"></i>Kennisgroepen:</p>
    <ul class="c-homepage__aside-list">

        {% if m.acl.user.s.hascollabmember as groups %}

            {% if groups|length <= 3 %}
                {% for kg in groups %}
                    <li><a href="{{ kg.page_url }}" class="c-homepage__aside-link">{{ kg.title }}</a></li>
                {% endfor %}
            {% else %}
                {% for kg in groups|slice:[1, 3] %}
                    <li><a href="{{ kg.page_url }}" class="c-homepage__aside-link">{{ kg.title }}</a></li>
                {% endfor %}
                <div id="show-all-groups" style="display: none;">
                    {% for kg in groups|slice:[4,] %}
                        <li><a href="{{ kg.page_url }}" class="c-homepage__aside-link">{{ kg.title }}</a></li>
                    {% endfor %}
                </div>
                {% include "show-hide-button.tpl" button_id="show-hide-groups-btn" %}
                {% wire
                    id="show-hide-groups-btn"
                    action={toggle target="show-all-groups"}
                %}
            {% endif %}

        {% else %}
            <li class="c-homepage__aside-link">Geen kennisgroepen</li>
        {% endif %}

    </ul>
</aside>
{% endif %}