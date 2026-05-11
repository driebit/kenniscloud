<ul class="share">
    <li>
        <a id="{{ #share_facebook }}" href="http://www.facebook.com/sharer.php?u=http%3A%2F%2F{{ m.site.hostname }}{{ id.page_url|urlencode }}&amp;t={{ id.title|urlencode }}" title="Facebook" class=""><i class="icon--facebook"></i></a>
        {% wire
            id=#share_facebook
            action={script script="return !window.open(this.href, 'Facebook', 'width=600,height=500,toolbar=0,location=0,scrollbars=0,status=0')"}
        %}
    </li>

    <li>
        <a id="mastodon-share-btn" data-src="{{id.title|urlencode}}&amp;url={{id.page_url_abs|urlencode}}" title="Mastodon" class=""><i class="icon--mastodon"></i></a>
    </li>

    <li>
        <a id="{{ #share_linkedin }}" href="https://www.linkedin.com/shareArticle?mini=true&amp;title={{ id.title|urlencode }}&amp;url={{ id.page_url_abs|urlencode }}" title="LinkedIn" class=""><i class="icon--linkedin"></i></a>
        {% wire
            id=#share_linkedin
            action={script script="return !window.open(this.href, 'LinkedIn', 'width=600,height=300,location=0,toolbar=0,scrollbars=0,status=0')"}
        %}
    </li>
    {% block other %}
        {% all include "share/share-link.tpl" %}
    {% endblock %}
</ul>
