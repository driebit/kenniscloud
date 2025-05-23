
<div class="u-d-flex u-flex-wrap u-flex-gap-1 u-padding-horizontal-mobile">
{% if id.o.hasextra_img %}
    {% button text=_"Images" class="c-btn--primary-no-icon" action={redirect dispatch="extra_images" id=id} %}
{% endif %}

{% if id.o.hasextra_doc %}
    {% button text=_"Documents" class="c-btn--primary-no-icon" action={redirect dispatch="extra_documents" id=id} %}
{% endif %}

{% if id.o.hasextra_rsc %}
    {% button text=_"External resources" class="c-btn--primary-no-icon" action={redirect dispatch="extra_resources" id=id} %}
{% endif %}

{% if id.o.hasextra_faq %}
    {% button text=_"FAQ" class="c-btn--primary-no-icon" action={redirect dispatch="extra_faqs" id=id} %}
{% endif %}
</div>