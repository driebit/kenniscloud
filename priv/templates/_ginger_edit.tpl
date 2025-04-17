{% overrules %}

{% block meta_extra %}
    <div class="row meta-extra" id="meta-extra" style="display:none; margin-left:0px;; margin-right:0px">
        <ul class="nav nav-tabs">
            {% block meta_tabs %}{% endblock %}
            {% if m.modules.info.mod_translation.enabled %}
                <li><a href="#meta-language" data-toggle="tab">{_ Language _}</a></li>
            {% endif %}
        </ul>
        <div class="tab-content">
            {% block meta_panels %}{% endblock %}
            <div class="tab-pane" id="meta-language">
                {% optional include "_translation_edit_languages.tpl" %}
            </div>
            {# <div class="tab-pane publication-dates" id="meta-pubdate">
                {% include "_edit_date.tpl" date=id.publication_start name="publication_start" is_end=0 %}
                {_ till _}
                {% include "_edit_date.tpl" date=id.publication_end name="publication_end" is_end=1 %}
            </div> #}
        </div>

        {% javascript %}
            $('#meta-extra .nav-tabs a:first').tab('show');
        {% endjavascript %}
    </div>
{% endblock %}
