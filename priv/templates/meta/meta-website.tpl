<ul class="person-details">
    {% if id.website %}
        <li class="person-detail--website">
            <span class="person-detail__label">
                <i class="icon--pointer"></i>
                {_ Website _}:
            </span>
            <a class="person-detail__link" href="{{ id.website }}" target="_new">{{ id.website }}</a>
        </li>
    {% endif %}
</ul>
