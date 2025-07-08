{% overrules %}
{# Overridden to simplify the dialog for the 'hasextra_***' predicates #}
{# rsc_props_title was also added to make 'hasextra_faq' title a required field #}

{% block rsc_props_title %}
    <div class="form-group label-floating">
        <input type="text" id="new_rsc_title" name="title"
                value="{{ title|escape }}" class="form-control do_autofocus"
                placeholder="{_ Title _}"
                {% if predicate == 'hasextra_faq' %}required{% endif %}
                autofocus {% if accept %}accept="{{ accept }}"{% endif %}>
        <label for="new_rsc_title">{_ Title _}</label>
    </div>
{% endblock %}

{% block new_rsc_header %}
    {% if predicate == 'hasextra_faq' %}
        <div class="form-group label-floating">
            {# Add a summary field (used for answers in 'frequently_asked_question's) #}
            <input required type="text" id="new_rsc_summary" name="summary"
                   value="{{ summary|escape }}" class="form-control do_autofocus"
                   placeholder="{_ Short answer _}"
            >
            <label for="new_rsc_summary">{_ Short answer _}</label>
        </div>
    {% elseif predicate == 'hasextra_rsc' %}
        <div class="form-group label-floating">
            <input type="text" id="new_rsc_reference" name="website"
                   value="{{ website|escape }}" class="form-control do_autofocus"
                   placeholder="{_ www.example.com _}"
            >
            <label for="new_rsc_reference">{_ Url or website _}</label>
        </div>
    {% else %}
        {% inherit %}
    {% endif %}
{% endblock %}

{% block category %}
    {# Automatically set the category #}
    {% if predicate == 'hasextra_faq' %}
        <input type="hidden" name="category_id" value="{{ m.rsc['frequently_asked_question'].id }}">
    {% elseif predicate == 'hasextra_rsc' %}
        <input type="hidden" name="category_id" value="{{ m.rsc['reference'].id }}">
    {% elseif predicate == 'hasextra_doc' %}
        <input type="hidden" name="category_id" value="{{ m.rsc['document'].id }}">
    {% elseif predicate == 'hasextra_img' %}
        <input type="hidden" name="category_id" value="{{ m.rsc['image'].id }}">
    {% elseif predicate == "has_subgroup" %}
        <input type="hidden" name="category_id" value="{{ m.rsc['acl_collaboration_group'].id }}">
    {% else %}
        {% inherit %}
    {% endif %}
{% endblock %}

{% block rsc_props %}
    {% if predicate == 'hasextra_faq' or predicate == 'hasextra_rsc' %}
        {# Automatically set as dependent #}
        <input type="hidden" name="is_dependent" value="1">
        <div class="form-group form__is_published">
            <label class="checkbox">
                <input type="checkbox" id="{{ #published }}" name="is_published" value="1"
                    {% if subject_id or m.admin.rsc_dialog_is_published %}checked{% endif %}>
                {_ Published _}
            </label>
        </div>
    {% else %}
        {% inherit %}
    {% endif %}
{% endblock %}
