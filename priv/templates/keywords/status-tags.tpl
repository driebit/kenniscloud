{% if id.status_label as label %}
    <div class="c-status-container {{ extraClass }}">
        <span class="kg-intro_status kg-intro_status--{{ label|lower }}">{_ Kennis Type _}: {{ label|translate }}</span>
    </div>
{% endif %}
