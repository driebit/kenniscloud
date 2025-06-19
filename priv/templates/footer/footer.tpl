<footer class="main-footer">
    <div class="main-footer__container">

    	<p class="main-footer__column--title">
			<b>KennisCloud</b>
			<img src="/lib/images/bibliotheek-logo.svg" alt="Een initiatief van: de Bibliotheek">
    	</p>

		{% if id.content_group_id != m.rsc.cg_methodology.id %}
			<div class="main-footer__column">
				<b>Info</b>
				{% menu menu_id=m.rsc.footer_menu.id id=id context=context menu_class="main-footer__nav" %}
			</div>

			{# <div class="main-footer__column">
				<b>Nieuwsbrief</b>

				<form action="" method="post" id="mc-embedded-subscribe-form" name="mc-embedded-subscribe-form" class="validate main-footer__newsletter" target="_blank" novalidate>

					<label for="mce-EMAIL">{_ E-mail _}</label>
					<input type="email" value="" name="EMAIL" placeholder="{_ Email address _}" class="required email" id="mce-EMAIL">
					<button type="submit" id="mc-embedded-subscribe" name="subscribe">{_ Signup _}</button>
					<div id="mce-responses" class="clear">
						<div class="response" id="mce-error-response" style="display:none"></div>
						<div class="response" id="mce-success-response" style="display:none"></div>
					</div>
					<div style="position: absolute; left: -5000px;" aria-hidden="true"><input type="text" name="b_4c2fe37761aef46295c1d9e55_6e692f7443" tabindex="-1" value=""></div>
				</form>
				<!--End mc_embed_signup-->
			</div> #}

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

