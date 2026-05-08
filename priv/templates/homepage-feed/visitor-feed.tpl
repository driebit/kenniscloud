<div class="c-homepage__feed-container">
    <div class="main-container u-d-flex--desktop u-justify-between">

        <div>
            <h2 class="bordered-title">{_ Recently published posts _}</h2>
            <h5>{_ Log in to see your personalized updates _}</h5>

            {% if m.search[{query cat=['event', 'contribution', 'news'] sort='-rsc.publication_start' is_published="true" pagelen=5 }] as results %}

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
