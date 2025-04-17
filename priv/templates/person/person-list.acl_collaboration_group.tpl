<div class="person-list">
    <h3 class="bordered-title">{{ title }}</h3>
    <div class="person-list__items" id="person-list__items-{{ #identifier }}">
        {% with id|kc_collaboration_group_members as members %}
            {% include "person/person-list-loop.tpl" result=members|slice:show context_rsc=id %}
            {% if members|length > show %}
                <a href="{% url `members` id=id %}" class="person--total">
                    <span>+ {{ remainder|make_list }}</span>
                </a>
            {% endif %}
        {% endwith %}

        {% include "page-actions/page-action-join-group.tpl"id=id %}
    </div>
</div>
