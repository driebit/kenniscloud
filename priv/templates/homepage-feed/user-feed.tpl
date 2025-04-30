<div class="c-homepage__feed-container">
    <div class="main-container u-d-flex--desktop u-flex-row-reverse u-justify-between">

        {% include "top-aside/top-aside.home.tpl" %}

        <div>
            <h2 class="bordered-title">Actuele updates en nieuws uit jouw kennisgroepen en regio's</h2>
            {% if m.search[{query cat=['event', 'contribution', 'remark'] sort='-rsc.created' is_published="true" content_group=m.acl.user.o.hascollabmember hasanyobject=m.acl.user.o.hasregion pagelen=5 }] as results %}

                <ul class="c-homepage__feed" id="results">
                    {% for rsc in results %}
                        {% catinclude "list/list-item-home.tpl" rsc.id %}
                    {% endfor %}
                </ul>

                {% button text="Meer updates" class="btn--primary u-margin-none" action={moreresults result=results
                                target="results"
                                catinclude
                                template="list/list-item-home.tpl"}
                %}
                
            {% else %}

                <ul class="c-homepage__feed" id="remarks">
                    {% include "list/list-item-home-empty.tpl" %}
                </ul>
            
            {% endif %}
        </div>
    </div>
</div>
