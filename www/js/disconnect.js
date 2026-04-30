/*
$(document).on('shiny:connected', function() {
  // Create a flag to track if the page is unloading (refreshing)
  window.is_unloading = false;
  
  window.addEventListener('beforeunload', function() {
    console.log('before unloading!!!');
    window.is_unloading = true;
  });

  // Listen for the disconnected state
  $(document).on('shiny:disconnected', function() {
    // If we are refreshing/unloading, hide the disconnect UI immediately
    if (window.is_unloading) {
      console.log('unloading!!!');
      hide_disconnect();
    }
  });
});
*/

// 1. Hide any potential disconnect overlays
function hide_disconnect() {
  var style = document.createElement('style');
  style.id = 'ss-hide-overlay-style';
  style.innerHTML = '#ss-overlay, #ss-connect-dialog { display: none !important; }';
  document.head.appendChild(style);
}

// HANDLE LOGIN/LOGOUT (HIDE DISCONNECT AND REDIRECT)
function hide_overlay(event, clicked_link_id, url) {
  var el = event.target.closest('#' + clicked_link_id);
  if (!el) return;

  event.preventDefault();
  hide_disconnect();
  window.location.href = window.AUTH_CONFIG[url];
}
document.addEventListener('click', (event) => hide_overlay(event, 'login_link', 'redirectUrl'));
document.addEventListener('click', (event) => hide_overlay(event, 'submit_sign_out', 'logoutUrl'));


// AUTH STATE
var logged_in = null;
var timeout = null;
var idleTimeout = 1000 * 60 * 9; //9 minutes

// OVERLAY
function showLogoutOverlay() {
  document.getElementById('logout-overlay').style.display = 'flex';
 
  setTimeout(function() {
    window.location.href = window.AUTH_CONFIG.logoutUrl;
  }, 1500);
}

// TIMER LOGIC (GATED)
function resetTimer() {
  if (logged_in !== true) return;
  clearTimeout(timeout);
  timeout = setTimeout(function() {
    hide_disconnect();
    showLogoutOverlay();
  }, idleTimeout);
}

// ACTIVITY HANDLER
function markActivity() {
  if (logged_in !== true) return;
  resetTimer();
}

// SHINY AUTH MESSAGE
Shiny.addCustomMessageHandler('auth_state', function(l) {
  logged_in = l;
  if (logged_in === true) resetTimer(); 
  document.getElementById('ss-hide-overlay-style')?.remove();
});

// USER ACTIVITY EVENTS
var events = ['click', 'mousemove', 'keypress', 'scroll'];

events.forEach(function(e) {
  document.addEventListener(e, markActivity, { passive: true });
});