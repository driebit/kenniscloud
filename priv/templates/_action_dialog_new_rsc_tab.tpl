{% overrules %}
{# Overridden to simplify the dialog for 'hasextra_faq'/'frequently_asked_question' #}

{% block new_rsc_header %}
    {% if predicate == 'hasextra_faq' %}
        <div class="form-group label-floating">
            {# Add a summary field (used for answers in 'frequently_asked_question's) #}
            <input type="text" id="new_rsc_summary" name="summary"
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
    {% else %}
        {% inherit %}
    {% endif %}
{% endblock %}

{% block rsc_props %}
    {% if predicate == 'hasextra_faq' %}
        {# Automatically set as dependent #}
        <input type="hidden" name="is_dependent" value="1">
        <div class="form-group form__is_published">
            <label class="checkbox">
                <input type="checkbox" id="{{ #published }}" name="is_published" value="1"
                    {% if subject_id or m.admin.rsc_dialog_is_published %}checked{% endif %}>
                {_ Published _}
            </label>
        </div>
    {% elseif predicate == 'hasextra_rsc' %}
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
