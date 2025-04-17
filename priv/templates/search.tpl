{% extends "base.tpl" %}

{% block title %}{_ Search _}: {{ q.qs|escape }}{% endblock %}

{% block body_class %}t--search{% endblock %}

{% block content %}

    <main class="">
        {% block search__top %}
            <div class="search__top">
                <div class="search__top__container">
                    <h1 class="page-title--search">{_ Search _}</h1>

                    <form class="search__top__form">
                        {% include "search/components/input-text.tpl" %}
                    </form>
                </div>
            </div>
        {% endblock %}

        <div class="search__container">
            <div id="search-results" class="search__results">
                <div id="search-list" class="search__results__list search__result__container">
                    {% with m.search.paged[{query
                        is_authoritative
                        is_findable
                        cat_exclude=['media', 'menu', 'remark', 'reference', 'categorization', 'category', 'predicate', 'acl_user_group', 'content_group', 'admin_content_query']
                        text=q.qs
                        page=q.page
                        pagelen=30
                    }] as result %}

                        {% include "search/list-wrapper.tpl" show_pager hide_showmore_button items=result %}

                    {% endwith %}
                </div>
            </div>
        </div>
    </main>

{% endblock %}
