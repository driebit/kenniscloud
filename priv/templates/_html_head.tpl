{% lib
    "bootstrap/css/bootstrap.css"
    "css/jquery.loadmask.css"
    "css/z.growl.css"
    "css/z.modal.css"
    "css/logon.css"
    "css/z.icons.css"
    "css/cookie-consent.css"
    "js/main.js"
%}

{# This is included here separately because
# 1. it should follow the other included CSSs for precedence/priority
# 2. to include fonts (e.g. Roboto) it has to be the first element in its `lib` tag
#}
{% lib "css/style.css" %}

<script src="//code.jquery.com/jquery-3.3.1.min.js"></script>

<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/fancybox/3.4.1/jquery.fancybox.min.css" />
<script src="https://cdnjs.cloudflare.com/ajax/libs/fancybox/3.4.1/jquery.fancybox.min.js"></script>
<script src='/lib/js/maplibre-gl.js'></script>
<link rel='stylesheet' href='https://unpkg.com/maplibre-gl@5.1.0/dist/maplibre-gl.css' />
<script src='https://unpkg.com/maplibre-gl@5.1.0/dist/maplibre-gl.js'></script>