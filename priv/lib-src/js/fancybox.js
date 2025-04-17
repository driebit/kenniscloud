$(document).ready(function(){
    // add listener so we can trap focus inside fancybox modal when showing video iframes
    window.addEventListener('blur', windowBlurred, false);
});

function windowBlurred(e) {
    const el = document.activeElement;

    // window will be blurred when entering an iframe
    if (el.tagName.toLowerCase() == 'iframe') {
        const caption = document.querySelector('.fancybox-caption');

        // checks if iframe is actually part of the fancybox modal
        if (caption) {
            // force visiblity on caption, otherwise we cannot focus on it
            caption.style.cssText = "visibility: visible !important;"

            // now we can add a listener to the caption...
            restoreFocus(caption)
        }
    }
}

function restoreFocus(caption) {
    // will focus on caption after iframe, because it is last in the DOM order
    caption.addEventListener('focusin', function() {
        // force visiblity on upper toolbar, and then it will automatically be next in the focus cycle!
        document.querySelector('.fancybox-toolbar').style.cssText = "visibility: visible !important;"
    })
}
