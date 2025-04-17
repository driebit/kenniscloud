<div class="page-header -news">
    <div class="page-intro">
        {% catinclude "category-of/category-of.tpl" id rsc_id=id.content_group_id %}

        {% include "page-title/page-title.tpl" %}

        {% include "subtitle/subtitle.tpl" %}

    	{% if id.publication_start as date %}
    	<time datetime="{{ date|date:"Y-F-jTH:i" }}">{{ date|date:"j.n.Y" }}</time>
    	{% endif %}

        {% include "page-actions/page-action-edit-thing.tpl" %}
    </div>
</div>
