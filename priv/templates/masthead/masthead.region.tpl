{% with
    main_content_class|default:"foldout",
    maptype|default:"map",
    recenter|default:"true",
    blackwhite|default:"true",
    main_content_class|default:"foldout"
as
    main_content_class,
    maptype,
    recenter,
    blackwhite,
    main_content_class
%}
    {% if id %}

        {% if id.depiction.id|is_a:`video` %}

            <div class="masthead--video">
                {% include "masthead/video.tpl" dep=id.depiction %}
            </div>

        {% else %}

            {% if id.o.hasbanner[1].depiction.width > 500 or id.o.header[1].depiction.width > 500 %}
                {% with id.o.hasbanner[1].depiction.id|default:id.o.header[1].depiction.id as dep %}
                    <div class="masthead do_parallax {{ extraClasses }}" style="background-image: url({% image_url dep mediaclass='masthead' crop=dep.crop_center %}); background-size: cover; background-position: {{ dep|background_position }};"></div>
                {% endwith %}
            {% elseif id.depiction.width > 500 and not (id.category.is_a.person or id.category.is_a.media) %}
                {% with id.depiction.id as dep %}
                    <div class="masthead do_parallax {{ extraClasses }}" style="background-image: url({% image_url dep mediaclass='masthead' crop=dep.crop_center %}); background-size: cover; background-position: {{ dep|background_position }};"></div>
                {% endwith %}
            {% elseif id.s.haspart.o.hasbanner[1].depiction.width > 500 or id.s.haspart[1].depiction.width > 500 %}
                {% with id.s.haspart.o.hasbanner[1].depiction.id|default:id.s.haspart[1].depiction.id as dep %}
                    <div class="masthead do_parallax {{ extraClasses }}" style="background-image: url({% image_url dep mediaclass='masthead' crop=dep.crop_center %}); background-size: cover; background-position: {{ dep|background_position }};"></div>
                {% endwith %}
            {% else %}
                <div class="masthead {{ extraClasses }}"></div>
            {% endif %}

        {% endif %}

    {% endif %}

{% endwith %}
