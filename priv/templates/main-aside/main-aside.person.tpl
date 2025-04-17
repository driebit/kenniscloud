<aside class="main-aside--person">
    <div class="main-container">
            {% if m.acl.user == id %}
                <h3 class="bordered-title">Alleen zichtbaar voor jou:<br>Instellingen mail</h3>

                {% include "person/person-email-preferences.tpl" %}
            {% endif %}

            {% if id.s.author %}
                <h3 class="bordered-title">{% if m.acl.user == id %}Mijn bijdragen{% else %}Bijdragen van {{ id.title|truncate:40 }}{% endif %}</h3>
                {% with m.search[{query hasobject=[id,'author'] cat_exclude="remark" pagelen=6 sort='-rsc.modified'}] as result %}

                    {% include "list/list.tpl" list_id="list--haspart" class="list--kg" list_template="list/list-item-kg.tpl" items=result extraClasses="" id=id noresults=" " %}

                {% endwith %}
            {% endif %}

            {% if id.o.like %}
                <h3 class="bordered-title">{% if m.acl.user == id %}Door mij gewaardeerd{% else %}Gewaardeerd door {{ id.title|truncate:40 }}{% endif %}</h3>
                {% with m.search[{query hassubject=[id,'like'] pagelen=6 sort='-rsc.modified'}] as result %}

                    {% include "list/list.tpl" list_id="list--haspart" class="list--kg" list_template="list/list-item-kg.tpl" items=result extraClasses="" id=id noresults=" " %}

                {% endwith %}
            {% endif %}
    </div>
</aside>
