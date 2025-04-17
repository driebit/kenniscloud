<a
 data-fancybox="gallery"
 data-caption="{% if id.summary %}{{ id.summary }}{% else %}Beeld {{ position }} van {{ total }}{% endif %}"
 href="{% image_url id mediaclass="lightbox-img" %}"
 class="lightbox"
 rel="carousel">
    <img src="{% image_url id mediaclass="carousel-img" %}" alt="{% if id.summary %}{{ id.summary }}{% else %}Beeld {{ position }} van {{ total }}{% endif %}">
</a>
