<div class="page-header -text">
    <div class="page-intro">
		{% button text=_"Back" class="btn--primary c-btn--back" action={redirect back} %}
        
        {% include "page-title/page-title.tpl" %}

        {% include "subtitle/subtitle.tpl" %}

        {% catinclude "keywords/keywords.tpl" id %}

        {% include "page-actions/page-action-edit-thing.tpl" %}
    </div>
</div>
