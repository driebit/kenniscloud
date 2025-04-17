{# This template exists only to override the one from 'zotonic_mod_cookie_consent'
## and as such avoid the insertion of the "css/cookie-consent.css".
##
## We instead include "css/cookie-consent.css" in the site's "_html_head.tpl",
## because in doing so we ensure that the site's "css/style.css" ends up
## following it and overriding its definitions (see "_cookies.scss").
#}