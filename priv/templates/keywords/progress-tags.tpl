{% if latest_contribution.is_a.event %}
    <div class="kg-intro_status kg-intro_status--meetup">
        <span>{_ Meetup _}</span>
    </div>
{% elif latest_contribution.progress_label as status %}
    <div class="kg-intro_status kg-intro_status--{{ status.title }}">
        <span>{{ status.title }}</span>
    </div>
{% endif %}
