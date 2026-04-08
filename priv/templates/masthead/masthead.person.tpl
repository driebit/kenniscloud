{% with id.o.hasbanner[1].depiction.id as dep %}
{% with id.o.depiction|last as avatar %}
    <div class="masthead--person {{ extraClasses }}">
        <div class="masthead--person__banner" style="background-image: url({% image_url dep mediaclass='masthead' crop=dep.crop_center %});"></div>
        <div class="main-container">
            {% image avatar.id mediaclass="profile_avatar" %}
            <div class="masthead__content {% if dep %}has-banner{% endif %}">
                <h1 class="page-title">
                    {% include "person/person-title.tpl" id=id %}
                </h1>
                <div class="u-d-flex u-flex-gap-1 u-d-justify-center--mobile">
                    {% catinclude "page-actions/page-actions.tpl" id %}
                </div>

                {% include "subtitle/subtitle.tpl" %}
                {% include "keywords/keywords.tpl" %}
            </div>
        </div>
    </div>
{% endwith %}
{% endwith %}
