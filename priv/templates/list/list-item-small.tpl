<li class="list-item-small">
    <a href="{{ r.uri|default:r.page_url }}" target="_blank">
        {% if r.og_image|default:r.depiction as dep %}
            <div class="list-item-small__img">
                {% if r.og_image %}
                    <img src="{{ r.og_image }}" alt="">
                {% else %}
                    <img src="{% image_url dep height="120" %}">
                {% endif %}
            </div>
        {% endif %}
        <div class="list-item-small__content">

            <small>{{ r.uri|default:r.page_url|split:"/"|slice:[3,3]|first|split:"."|reversed|slice:[1,2]|reversed|join:"." }}</small>

            <h3>{{ r.og_title|default:r.title|truncate:100 }}</h3>

            <p>{{ r.og_description|default:r.body|truncate:100 }}</p>

            <span>Lees meer</span>
        </div>
    </a>
</li>
