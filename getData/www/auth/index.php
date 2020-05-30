<?php
session_start();

date_default_timezone_set('Europe/Paris');


echo "
<script>
var baseUrl = 'https://https://dev-969232.okta.com/';
var xhr = new XMLHttpRequest();
if ('withCredentials' in xhr) {
    xhr.onerror = function() {
      alert('Invalid URL or Cross-Origin Request Blocked.  You must explicitly add this site (' + window.location.origin + ') to the list of allowed websites in the administrator UI');
    }
    xhr.onload = function() {
        alert(this.responseText);
    };
    xhr.open('GET', baseUrl + '/api/v1/users/me', true);
    xhr.withCredentials = true;
    xhr.send();
} else {
    alert('CORS is not supported for this browser!')
}
</script>
";


function http($url, $params=false) {
  $ch = curl_init($url);
  curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
  if($params)
    curl_setopt($ch, CURLOPT_POSTFIELDS, http_build_query($params));
  return json_decode(curl_exec($ch));
}

if(isset($_GET['logout'])) {
  unset($_SESSION['username']);
  header('Location: /auth/');
  die();
}

if(isset($_SESSION['username'])) {
  echo '<p>Logged in as</p>';
  echo '<p>' . $_SESSION['username'] . '</p>';
  echo '<p><a href="/auth/?logout">Log Out</a></p>';
  die();
}

$client_id = '0oabqx4g3wHtEx1oO4x6';
$client_secret = '7-9Dip1kTZSrIusyf9R2Pz-Do1j7_YPzr2VdEbB2';
$redirect_uri = 'http://10.0.0.104/auth/';
$metadata_url = 'https://dev-969232.okta.com/oauth2/default/.well-known/oauth-authorization-server';
$metadata = http($metadata_url);

if(isset($_GET['code'])) {

  if($_SESSION['state'] != $_GET['state']) {
    die('Authorization server returned an invalid state parameter');
  }

  if(isset($_GET['error'])) {
    die('Authorization server returned an error: '.htmlspecialchars($_GET['error']));
  }

  $response = http($metadata->token_endpoint, [
    'grant_type' => 'authorization_code',
    'code' => $_GET['code'],
    'redirect_uri' => $redirect_uri,
    'client_id' => $client_id,
    'client_secret' => $client_secret,
  ]);

  if(!isset($response->access_token)) {
    die('Error fetching access token');
  }

  $token = http($metadata->introspection_endpoint, [
    'token' => $response->access_token,
    'client_id' => $client_id,
    'client_secret' => $client_secret,
  ]);

  if($token->active == 1) {
    $_SESSION['username'] = $token->username;
    header('Location: /');
    die();
  }
}

if(!isset($_SESSION['username'])) {
  $_SESSION['state'] = bin2hex(openssl_random_pseudo_bytes(5));

  $authorize_url = $metadata->authorization_endpoint.'?'.http_build_query([
    'response_type' => 'code',
    'client_id' => $client_id,
    'redirect_uri' => $redirect_uri,
    'state' => $_SESSION['state'],
    'scope' => 'openid',
  ]);

  echo '<p>Not logged in</p>';
  echo '<p><a href="'.$authorize_url.'">Log In</a></p>';
}
