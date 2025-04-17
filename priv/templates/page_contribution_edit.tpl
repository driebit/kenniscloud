{% extends "page_ginger_edit.contribution.tpl" %}

{% block title %}{_ Edit _} {{ m.rsc["story"].title }}{% endblock%}

{% block container %}
    {% with "contribution" as category %}
    {% with id|temporary_rsc:{props
            category_id=category
            content_group_id=m.rsc[q.kennisgroep].id
            is_published=0
        } as id
    %}
        {% inherit %}
    {% endwith %}
    {% endwith %}
{% endblock %}
