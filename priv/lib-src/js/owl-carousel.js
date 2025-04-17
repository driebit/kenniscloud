$(document).ready(function(){

    const carouselChildren = $(".owl-carousel").attr('items');

    $(".owl-carousel").owlCarousel({
        nav: true,
        items: 2,
        loop: false,
        margin: 0,
        dots: false,
        responsive: {
            600: {
                nav: true,
                items: 3,
                loop: carouselChildren > 3
            },
            800: {
                nav: true,
                items: 4,
                loop: carouselChildren > 4
            }
        }
    });

    // nav buttons are for mouse users only (in this website's context),
    // so hide them here from assistive technologies:
    if ($(".owl-carousel")[0]) {
        $(".owl-prev")[0].setAttribute("aria-hidden", true);
        $(".owl-next")[0].setAttribute("aria-hidden", true);
    }

    // make sure that initial :focus is on something that is in view
    $.each($(".cloned > a"), function() {
        this.setAttribute("tabindex", "-1");
    });
});
