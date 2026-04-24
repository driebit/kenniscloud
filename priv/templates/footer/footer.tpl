<footer class="main-footer">
    <div class="main-footer__container">

    	<p class="main-footer__column--title">
			<b>KennisCloud</b>
			<span class="c-footer__logo-wrapper">
				Een initiatief van:
				<img src="/lib/images/--driebit-logo.svg" class="c-footer__logo" alt="Een initiatief van: Driebit">
			</span>
    	</p>

		{% if id.content_group_id != m.rsc.cg_methodology.id %}
			<div class="main-footer__column">
				<b>Info</b>
				{% menu menu_id=m.rsc.footer_menu.id id=id context=context menu_class="main-footer__nav" %}
			</div>

			<div class="main-footer__column">
				<b>Volg ons op</b>

				<ul class="main-footer__social">
					<li>
						<a href="https://mastodon.social/@kenniscloud " class="main-footer__social__btn" target="_blank"><i class="icon--mastodon-other"></i>Mastodon</a>
					</li>
				</ul>

			</div>
		{% endif %}
    </div>
</footer>

