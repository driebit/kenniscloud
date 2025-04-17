<div class="c-homepage__feed-container">
    <div class="main-container u-d-flex--desktop u-justify-between">

        <div>
            <h2 class="bordered-title">KennisCloud Nieuws</h2>

            {% if m.search[{query cat='remark' sort='-rsc.modified' is_published="true" pagelen=5 }] as remarks %}

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

                <ul class="c-homepage__feed">
                    {% include "list/list-item-home-empty.tpl" %}
                </ul>
                
            {% endif %}
        </div>
    </div>
</div>