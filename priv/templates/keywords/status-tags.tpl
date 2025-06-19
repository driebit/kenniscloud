{% if id.status_label as label %}
    <div class="c-status-container">
        <span class="kg-intro_status kg-intro_status--{{ label|lower }}">{{ label|translate }}</span>
    </div>
{% endif %}
