<nav class="global-nav do_global_nav {% block global_nav_extra_classes %}{% endblock %}">
    <div class="global-nav__container">
        {% block logo %}
            <a href="/" class="global-nav__logo">
                <img src="/lib/images/cloud.svg" alt=""> KennisCloud
            </a>
        {% endblock %}

        {% block search_bar %}
            {% include "search-suggestions/search-form.tpl"
                extraFormClassess=extraFormClassess formclass="c-nav-search"
            %}
        {% endblock %}

        {% block menu %}
            {% menu id=id %}
        {% endblock %}
    </div>

    {% block actions %}
        {% include "global-nav/global-nav-actions.tpl" %}
    {% endblock %}
</nav>
