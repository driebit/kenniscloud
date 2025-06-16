{% if latest_contribution.is_a.event %}
    <div class="kg-intro_status kg-intro_status--status_keyword_meetup">
        <span>{_ Meetup _}</span>
    </div>
{% elif latest_contribution.status as status %}
    <div class="kg-intro_status kg-intro_status--{{ status.name }}">
        <span>{{ status.title }}</span>
    </div>
{% endif %}

{% if id.status_label as label %}
    <div class="c-status-container">
        <span class="kg-intro_status kg-intro_status--{{ label|lower }}">{{ label|translate }}</span>
    </div>
{% endif %}
