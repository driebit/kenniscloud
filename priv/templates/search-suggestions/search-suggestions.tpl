{% if result %}
    {%
        with
            result|group_by:`category_id`,
            [`acl_collaboration_group`, `event`, `contribution`, `rest`]
        as
            grouped_result,
            category_order
    %}
        {% for filter_cat in category_order %}
            {% for results_of_cat in grouped_result %}
                {% with results_of_cat[1].category_id as results_cat %}
                    {%
                        if results_cat.name == filter_cat
                            or (filter_cat == `rest`
                                and not(results_cat.name|as_atom|member:category_order))
                    %}
                        <h4 class="search-suggestions__suggestions__title">{{ results_cat.title }}</h4>
                        <ul>
                            {% for id in results_of_cat %}
                                <li>
                                    <a href="{{ id.page_url }}">{{ id.title }}</a>
                                </li>
                            {% endfor %}
                        </ul>
                    {% endif %}
                {% endwith %}
            {% endfor %}
        {% endfor %}
    {% endwith %}
{% else %}
    <h4 class="search-suggestions__suggestions__title no-results">{_ Nothing found _}</h4>
{% endif %}

