<div class="u-d-flex u-flex-col u-flex-gap-2">
    <div class="person-list">
        {% if m.acl.user %}
            <h3 class="bordered-title">Dit zijn de Community Librarians in {{ id.address_city|truncate:35 }}</h3>
            <div class="person-list__items" id="person-list__items-{{ #identifier }}">
                {% for librarian in m.kc_region[id].community_librarians %}
                    {% include "list/list-item-person-small.tpl" id=librarian %}
                {% endfor %}
            </div>


            <a href="mailto:{{ id.email }}" class="kg-intro__right__link">Neem contact op met de Community Librarians</a>
        {% else %}
            <h3 class="bordered-title">Word lid van KennisCloud</h3>

            <p>Praat en denk mee over relevante onderwerpen, deel je kennis en leer van de kennis van anderen!</p>

            <a href="{% url logon p={page id=id}|url %}" class="btn--primary">Sluit je bij ons aan</a>
        {% endif %}
    </div>

    {% live template="page-actions/page-action-join-region.tpl" id=id topic={subject id=id predicate=`hasregion`} %}

</div>