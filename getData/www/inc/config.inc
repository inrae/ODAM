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

#---------------------------
# Get authorization key code
#---------------------------
if (! preg_match('/(curl|python-requests)/', $_SERVER['HTTP_USER_AGENT']))
  # If it is not through a CURL library, get the saved API-Key if exists
   $auth0 = isset($_SESSION['api-key'])       ? $_SESSION['api-key']       : '' ;
else
  # otherwise, no default API-Key
   $auth0 = '';
# Get the API-Key if provided in Header
$auth1 = isset($_SERVER['HTTP_X_API_KEY']) ? $_SERVER['HTTP_X_API_KEY'] : $auth0;
# Get the API-Key if provided as parameter (query string)
$auth = isset($_GET['auth'])               ? $_GET['auth']              : $auth1 ;

# If it is not through a CURL library, it means it is probably a web browser
# So save API-Key in the session
if (! preg_match('/(curl|python-requests)/', $_SERVER['HTTP_USER_AGENT']))
    $_SESSION['api-key'] = $auth;

# Set authorization message
if (isset($_GET['auth']))
   $auth_msg='authorization by url query';
elseif (isset($_SERVER['HTTP_X_API_KEY']))
   $auth_msg='authorization fixzed by url header';
else
   $auth_msg='authorization fixed by rules';


# X_Forwarded - get first IP
if (strpos($_SERVER['HTTP_X_FORWARDED_FOR'], ',')) {
   $arr = explode(',',$_SERVER['HTTP_X_FORWARDED_FOR']);
   $X_Forwarded = trim($arr[0]);
} else
   $X_Forwarded = $_SERVER['HTTP_X_FORWARDED_FOR'];

# Internal Session Identifier
$SESSID  = md5(uniqid(rand(),true));

# Environment variables
$ENV = array( 'OS' => $OS,
              'DATADIR' => $set->DATAROOT,
              'TMPDIR' => $TMPDIR,
              'SESSID' => $SESSID,
              'QSQL' => $QSQL,
              'SQLITE' => $SQLITE,
              'SQLOPT' => '-tabs',
              'DBNAME' => 'sqlite3.db',
              'TRANSAC' => 0,
              'FIELD_LIMIT' => -1,
              'EXTFILES' => $set->EXTFILES,
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
