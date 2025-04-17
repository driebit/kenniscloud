<div class="filters">
	<button class="btn--filters">
		Filter
	</button>
	<div class="filters__inner">
		<form id="filter_form" method="post" action="postback" class="filter-form do_filter_form">
			{% if m.search[{query cat="keyword" sort="pivot_title"}] as result %}
				<h3>Tags</h3>
				<ul class="keywords">
					{% for id in result %}
						<li>
							<input type="checkbox" name="filter_location" value="{{ m.rsc[id].id }}" class="members__filter__keyword" id="{{ m.rsc[id].title|slugify }}">
							<label for="{{ m.rsc[id].title|slugify }}" class="btn--secondary" title="{{ m.rsc[id].title }}">{{ m.rsc[id].title|truncate:12 }}</label>
						</li>
					{% endfor %}
				</ul>
			{% endif %}
			{% if m.search[{query cat="location" sort="pivot_title"}] as result %}
				<h3>Locaties</h3>
				<ul class="keywords">
					{% for id in result %}
						<li>
			                <input type="checkbox" name="filter_location" value="{{ m.rsc[id].id }}" class="members__filter__location" id="{{ m.rsc[id].title|slugify }}">
							<label for="{{ m.rsc[id].title|slugify }}" class="btn--secondary" title="{{ m.rsc[id].title }}">{{ m.rsc[id].title|truncate:12 }}</label>
						</li>
					{% endfor %}
				</ul>
			{% endif %}
		</form>
	</div>
</div>
{% wire id="filter_form" type="submit" postback={filter_form} delegate=`kenniscloud` %}
