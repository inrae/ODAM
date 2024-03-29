<?php
/* ID config.inc v1.2
 * Copyright (C) 2015-2021 INRAE - DJ
 *
    This file is part of ODAM

    ODAM is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    ODAM is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with ODAM.  If not, see <http://www.gnu.org/licenses/>.
*/

include_once ('inc/Set_Config.inc');

#----------------------------------------------
# Populate some variables 
#----------------------------------------------

# Configuration variables depending on OS
$ROOTDIR  = $_SERVER['DOCUMENT_ROOT'];
if (strtoupper(substr(PHP_OS, 0, 3)) === 'WIN') {
    $set = new Set_Config(array('-conf'=>"$ROOTDIR/../settings/getdata.conf",'-global'=>FALSE));
    $OS = isset($set->OS) ? $set->OS : 'WIN';
}
else {
    $set = new Set_Config(array('-conf'=>"$ROOTDIR/settings/getdata.conf",'-global'=>FALSE));
    $OS = isset($set->OS) ? $set->OS : 'LINUX';
}

# URL Proxy for the Getdata API
$GETDATA_URL_PROXY = getenv("GETDATA_URL_PROXY", true) ? getenv("GETDATA_URL_PROXY") : $set->GETDATA_URL_PROXY;

# Environment variables depending on OS
if ($OS=='LINUX') {
    $TMPDIR  = "$ROOTDIR/tmp";
    $QSQL    = $set->QBIN.' '.$set->QOPTIONS;
    $SQLITE  = $set->SQLITE;
    $BB      = '';
} else {
    $TMPDIR  = str_replace( '\\', '/', getenv('TEMP'));
    $QSQL    = $ROOTDIR.'/../bin/q '.$set->QOPTIONS;
    $SQLITE  = $ROOTDIR.'/../bin/sqlite3.exe';
    $BB      = $ROOTDIR.'/../bin/busybox.exe';
}

# Indicate if metadata can be obtained even if there is no authorization on the data
# By default, metadata has to be available to satisfy the FAIR principes
$METADATA_ACCESS = isset($set->METADATA_ACCESS) ? $set->METADATA_ACCESS : 1;

# Allow API to use the IP of the client passed through the header for authentication
$IP_IN_HEADER = isset($set->VPN) ? $set->VPN : 0;
$IP_IN_HEADER = getenv("VPN", true) ? getenv("VPN") : $IP_IN_HEADER;

#---------------------------
# Get authorization key code
#---------------------------
if (! preg_match('/(curl|python-requests)/', $_SERVER['HTTP_USER_AGENT']))
  # If it is not through a CURL library, get the saved API-Key if exists
   $auth0 = isset($_SESSION['api-key']) ? $_SESSION['api-key'] : '' ;
else
  # otherwise, no default API-Key
   $auth0 = '';
# Get the API-Key if provided in Header
$auth1 = isset($_SERVER['HTTP_X_API_KEY']) ? $_SERVER['HTTP_X_API_KEY'] : $auth0;
# Get the API-Key if provided as parameter (query string)
$auth = isset($_GET['auth']) ? $_GET['auth'] : $auth1 ;

# If it is not through a CURL library, it means it is probably a web browser
# So save API-Key in the session
if (! preg_match('/(curl|python-requests)/', $_SERVER['HTTP_USER_AGENT']))
    $_SESSION['api-key'] = $auth;

# Set authorization message
if (isset($_GET['auth']))
   $auth_msg='authorization fixed by url query';
elseif (isset($_SERVER['HTTP_X_API_KEY']))
   $auth_msg='authorization fixed by url header';
else
   $auth_msg='authorization fixed by rules';

#---------------------------
# X_Forwarded - get the IP
#---------------------------
$X_Forwarded = '';
if ($IP_IN_HEADER && isset($_SERVER['HTTP_X_API_IP']) && strlen($_SERVER['HTTP_X_API_IP']))
   $X_Forwarded=$_SERVER['HTTP_X_API_IP'];
elseif (isset($_SERVER['HTTP_X_FORWARDED_FOR']) && strlen($_SERVER['HTTP_X_FORWARDED_FOR']))
   $X_Forwarded=$_SERVER['HTTP_X_FORWARDED_FOR'];
elseif (isset($_SERVER['HTTP_X_FORWARDED_HOST']) && strlen($_SERVER['HTTP_X_FORWARDED_HOST']))
   $X_Forwarded=$_SERVER['HTTP_X_FORWARDED_HOST'];
elseif (isset($_SERVER['HTTP_X_FORWARDED_SERVER']) && strlen($_SERVER['HTTP_X_FORWARDED_SERVER']))
   $X_Forwarded=$_SERVER['HTTP_X_FORWARDED_SERVER'];

if (strpos($X_Forwarded, ',')) {
   $arr = explode(',',$X_Forwarded);
   $X_Forwarded = trim($arr[0]);
}

# Internal Session Identifier
$SESSID  = md5(uniqid(rand(),true));


#---------------------------
# Environment variables
#---------------------------
$ENV = array( 'OS' => $OS,
              'DATADIR' => $set->DATAROOT,
              'TMPDIR' => $TMPDIR,
              'SESSID' => $SESSID,
              'QSQL' => $QSQL,
              'SQLITE' => $SQLITE,
              'SQLOPT' => '-tabs',
              'DBNAME' => 'sqlite3.db',
              'CHECKSUM' => 'checksum',
              'TRANSAC' => 0,
              'FIELD_LIMIT' => -1,
              'EXTFILES' => $set->EXTFILES,
              'METADATA_ACCESS' => $METADATA_ACCESS,
              'X_API_KEY' => $_SERVER['HTTP_X_API_KEY'],
              'X_REAL_IP' => $_SERVER['HTTP_X_REAL_IP'],
              'X_Forwarded' => $X_Forwarded,
              'HOST' => $_SERVER['HTTP_HOST'],
              'GETDATA_URL_PROXY' => $GETDATA_URL_PROXY );

# Datapackage information
$DPKG_INFOS = array( 'URL_JSONDEF' => $set->URL_JSONDEF,
                     'DATAPACKAGE_VERSION' => $set->DATAPACKAGE_VERSION,
                     'LICENSE' => $set->LICENSE );

$stderr = "$TMPDIR/$SESSID.err";
$stdout = "$TMPDIR/$SESSID.out";
$sep = "\t";

?>
