/*PURPOSE: Script to redirect user if auth cookie is found*/
function clear_cookie() {
  document.cookie = 'cognito_session=;expires=Thu, 01 Jan 1970 00:00:00 UTC;path=/';
}

/*Check for `cognito_session` cookie*/
const hasSession = document.cookie.includes('cognito_session=active');

/*Once authenticated, Cognito generates a secure, single-use, 
highly encrypted Authorization Code on AWS's servers
ORR makes a secure, behind-the-scenes HTTP request back to Cognito to exchange 
it for the actual user details and Access Tokens*/
const hasCode = window.location.search.includes('code=');

/*If something goes wrong on Cognito's end (for example, the user clicks "Cancel", 
or they are denied access), Cognito will redirect them back with an error in the 
URL instead of a code: ?error=access_denied.
Because our UI JavaScript only checks for !hasCode, it will instantly redirect 
the user back to Cognito, which will instantly return the error again, creating 
an infinite flashing loop that crashes the browser tab.*/
const hasError = window.location.search.includes('error=');


// If they have an active session cookie, but NO code in the URL, redirect instantly!
if (hasSession && !hasCode && !hasError) {
  window.location.href = window.AUTH_CONFIG.redirectUrl;
}