// Generate a share link for the user's Mastodon domain 
function MastodonShare(e){

    var btn = document.getElementById("mastodon-share-btn");
    var src = btn.getAttribute("data-src");
    
    var domain = prompt("Welk Mastodon domein gebruik je?", "mastodon.social");
    if (domain == "" || domain == null){
        return;
    }

    var url = "https://" + domain + "/share?text=" + src;
    window.open(url, '_blank');
}

// Wait for the DOM to be parsed before querying elements
document.addEventListener("DOMContentLoaded", function() {
  var btn = document.getElementById("mastodon-share-btn");
  if (btn) {
    btn.addEventListener("click", MastodonShare);
  }
});
