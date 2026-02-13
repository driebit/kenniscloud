<aside class="main-aside--person">
    <div class="main-container">
            {% if m.acl.user == id %}
                <h3 class="bordered-title">Alleen zichtbaar voor jou:<br>Instellingen mail</h3>

                {% include "person/person-email-preferences.tpl" %}
            {% endif %}

            {# Used to be querying id.s.author, though that doesn't work anymore, because
               the author connection is not automatically added to bijdragen anymore.
               Might be intentional. Should be a search on _either_ creator_id, modifier_id
               or author edge, but it is not clear how to create or-queries.
            #}
            {% if m.search[{query filter=[['creator_id', id], ['modifier_id', id]] pagelen=6 sort='-rsc.modified' cat=['contribution']}] as result %}
                <h3 class="bordered-title">{% if m.acl.user == id %}Mijn bijdragen{% else %}Bijdragen van {{ id.title|truncate:40 }}{% endif %}</h3>

                {% include "list/list.tpl" list_id="list--haspart" class="list--kg" list_template="list/list-item-kg.tpl" items=result extraClasses="" id=id noresults=" " %}
            {% endif %}

            {% if id.o.like %}
                <h3 class="bordered-title">{% if m.acl.user == id %}Door mij gewaardeerd{% else %}Gewaardeerd door {{ id.title|truncate:40 }}{% endif %}</h3>
                {% with m.search[{query hassubject=[id,'like'] pagelen=6 sort='-rsc.modified'}] as result %}

                    {% include "list/list.tpl" list_id="list--haspart" class="list--kg" list_template="list/list-item-kg.tpl" items=result extraClasses="" id=id noresults=" " %}

                {% endwith %}
            {% endif %}
    </div>
</aside>
