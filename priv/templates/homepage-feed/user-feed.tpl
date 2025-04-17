<div class="c-homepage__feed-container">
    <div class="main-container u-d-flex--desktop u-flex-row-reverse u-justify-between">

        {% include "top-aside/top-aside.home.tpl" %}

        <div>
            <h2 class="bordered-title">Actuele updates en nieuws uit jouw kennisgroepen en regio's</h2>

            {% if m.search[{query cat='remark' sort='-rsc.modified' is_published="true" pagelen=5 content_group=m.acl.user.s.hascollabmember }] as remarks %}

                <ul class="c-homepage__feed" id="remarks">
                    {% for remark in remarks %}
                        {% include "list/list-item-home.tpl" id=remark %}
                    {% endfor %}
                </ul>

                {% button text="Meer updates" class="btn--primary u-margin-none" action={moreresults result=remarks
                                target="remarks"
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
