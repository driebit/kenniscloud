{# vars:
    - containerClass
    - titleClass
    - title
    - result
    - list_item_template
 #}
{% with
    containerClass,
    titleClass,
    title,
    result,
    list_item_template|default:"_driebit_timeline-item.tpl"
as
    containerClass,
    titleClass,
    title,
    result,
    list_item_template
%}
    {% if result %}
        <div class="{{ containerClass }}">
            <h3 class="{{ titleClass }}">{{ title }}</h3>
            <div class="c-timeline">
                <div class="c-timeline__line"></div>
                {% for rsc in result|is_visible %}
                    {% catinclude list_item_template rsc.id %}
                {% endfor %}
            </div>
        </div>
    {% endif %}

    <script type="application/javascript" src="/lib/js/timeline.js" ></script>
{% endwith %}
