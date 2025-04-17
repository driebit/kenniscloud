{# In zotonic1 the config values are not public, so we created an erl model to get the matomo value (m_matomo_config.erl) #}
{% if m.matomo_config.mtm_container_id as mtm_id %}
    <!-- Matomo Tag Manager -->
    <script type="text/x-cookie-consent" data-cookie-consent="stats" nonce="{{ m.req.csp_nonce }}">
        var _mtm = window._mtm = window._mtm || [];
        _mtm.push({'mtm.startTime': (new Date().getTime()), 'event': 'mtm.Start'});
        var d=document, g=d.createElement('script'), s=d.getElementsByTagName('script')[0];
        g.async=true; g.src='{{  m.matomo_config.mtm_url |default:"https://cdn.matomo.cloud/driebit.matomo.cloud" }}/container_{{ mtm_id }}.js'; s.parentNode.insertBefore(g,s);
    </script>
    <!-- End Matomo Tag Manager -->
{% endif %}