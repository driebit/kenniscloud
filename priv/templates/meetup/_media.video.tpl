{% if id.medium.mime == "text/html-oembed" or id.medium.mime == "text/html-video-embed" %}

    {% if id.medium.mime == "text/html-video-embed" %}

        {# embed code used #}

        {% if id.medium.video_embed_service|lower == "youtube" or id.medium.video_embed_service|lower == "vimeo" %}

            {% with id.medium.video_embed_code|replace:["\n", ""]|replace:["^.*src=\"([^\"]+).*$", "\\1"] as embed_code %}

            {# ACCESSIBILITY: FANCYBOX MUST USE DATA-CAPTION with tabindex='0', or else :focus will leave page #}
                <a
                data-fancybox="gallery"
                data-caption="<span tabindex='0'>{% if id.summary %}{{ id.summary }}{% endif %}</span>"
                href="{{ embed_code }}"
                class="lightbox lightbox-video-embed fancybox.iframe"
                rel="carousel"
                title="{% if id.title %}<h4>{{ id.title }}</h4>{% endif %} {% if id.summary %}<p>{{ id.summary }}</p>{% endif %}">
                    {% image id mediaclass="carousel-img" title=id.title alt=id.summary|default:id.title %}
                    {% if caption and id.summary %}
                        <div class="attached-media__caption">{{ id.summary }}</div>
                    {% endif %}
                </a>

            {% endwith %}

        {% endif %}

    {% elif id.medium.mime == "text/html-oembed" %}

        {# assume share url used #}

        {% if id.medium.oembed.provider_name|lower == "youtube" %}

            <a
            data-fancybox="gallery"
            data-caption="<span tabindex='0'>{% if id.summary %}{{ id.summary }}{% endif %}</span>"
            href="{{ id.medium.oembed_url|replace:["watch\\?v=","embed/"]|replace:["youtu.be/", "youtube.com/embed/"] }}"
            class="lightbox lightbox-video-embed fancybox.iframe"
            rel="carousel"
            title="{% if id.title %}<h4>{{ id.title }}</h4>{% endif %} {% if id.summary %}<p>{{ id.summary }}</p>{% endif %}">
                {% image id mediaclass="carousel-img" title=id.title alt=id.summary|default:id.title %}
            </a>

        {% elif id.medium.oembed.provider_name|lower == "vimeo" %}

            <a
            data-fancybox="gallery"
            data-caption="<span tabindex='0'>{% if id.summary %}{{ id.summary }}{% endif %}</span>"
            href="https://player.vimeo.com/video/{{ id.medium.oembed.video_id }}"
            class="lightbox lightbox-video-embed fancybox.iframe"
            rel="carousel"
            title="{% if id.title %}<h4>{{ id.title }}</h4>{% endif %} {% if id.summary %}<p>{{ id.summary }}</p>{% endif %}">
                {% image id mediaclass="carousel-img" title=id.title alt=id.summary|default:id.title %}
            </a>

        {% endif %}

    {% endif %}

{% else %}

    {# MP4 of rdf #}

    {% if m.rdf[id.rdf] as rdf %}

        {% if rdf.uri|match:".*youtube.*" %}
            <a
            data-fancybox="gallery"
            data-caption="<span tabindex='0'>{{ rdf.description }} {{ rdf.rights }}</span>"
            href="{{ rdf.uri|replace:["watch\\?v=","embed/"]|replace:["youtu.be/", "youtube.com/embed/"] }}"
            class="lightbox lightbox-video-embed fancybox.iframe"
            rel="carousel"
            title="{{ rdf.description }}">
                {% image id mediaclass="carousel-img" title=id.title alt=rdf.description %}
            </a>
        {% else %}
            <a href="{{ rdf.uri }}" class="" rel="carousel">
                {% image id mediaclass="carousel-img" alt=rdf.description %}
                <div class="attached-media__caption">{{ rdf.description }} {{ rdf.rights }}</div>
            </a>
        {% endif %}

    {% else %}
        <a
        data-fancybox="gallery"
        data-caption="<span tabindex='0'>{% if id.summary %}{{ id.summary }}{% endif %}</span>"
        href="#"
        data-video-url="/media/attachment/{{ id.medium.filename }}"
        data-video-width="{{ id.medium.width }}"
        data-video-height="{{ id.medium.height }}"
        class="lightbox lightbox-video-embed default-video-player"
        rel="carousel"
        title="{% if id.title %}<h4>{{ id.title }}</h4>{% endif %} {% if id.summary %}<p>{{ id.summary }}</p>{% endif %}">
            {% image id mediaclass="carousel-img" alt=id.summary|default:id.title %}
        </a>
    {% endif %}
{% endif %}
