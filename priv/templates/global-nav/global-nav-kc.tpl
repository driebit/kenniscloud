<nav class="global-nav do_global_nav {% block global_nav_extra_classes %}{% endblock %}">
    <div class="global-nav__container">
        {% block logo %}
            <a href="/" class="global-nav__logo">
                KennisCloud
            </a>
        {% endblock %}

        {% block menu %}
            {% menu id=id %}
        {% endblock %}

        {% block search_bar %}
            {% include "search-suggestions/search-form.tpl"
                extraFormClassess=extraFormClassess formclass="c-nav-search"
            %}
        {% endblock %}

        {% block actions %}
            {% include "global-nav/global-nav-actions.tpl" %}
        {% endblock %}
    </div>

</nav>
