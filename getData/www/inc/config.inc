<?php
/* ID config.inc v1.2
 * Copyright (C) 2021 INRA - DJ

 */

include_once ('inc/Set_Config.inc');

#----------------------------------------------
# Populate some variables 
#----------------------------------------------

$ROOTDIR  = $_SERVER['DOCUMENT_ROOT'];
if (strtoupper(substr(PHP_OS, 0, 3)) === 'WIN') {
    $set = new Set_Config(array('-conf'=>"$ROOTDIR/../settings/getdata.conf",'-global'=>FALSE));
    $OS = isset($set->OS) ? $set->OS : 'WIN';
}
else {
    $set = new Set_Config(array('-conf'=>"$ROOTDIR/settings/getdata.conf",'-global'=>FALSE));
    $OS = isset($set->OS) ? $set->OS : 'LINUX';
}

# DEBUG status
$DEBUG = isset($set->DEBUG) ? $set->DEBUG : 0 ;

$GETDATA_URL_PROXY = isset($_ENV["GETDATA_URL_PROXY"]) ? $_ENV["GETDATA_URL_PROXY"] : $set->GETDATA_URL_PROXY;

$URLSERVER = preg_replace('/\/$/', '', $GETDATA_URL_PROXY);

$DATADIR  = $set->DATAROOT;

if ($OS=='LINUX') {
    $TMPDIR  = "$ROOTDIR/tmp";
    $QSQL    = $set->QBIN.' '.$set->QOPTIONS;
    $AUTHKEY  = $set->AUTHKEY;
} else {
    $TMPDIR  = str_replace( '\\', '/', getenv('TEMP'));
    $QSQL     = $ROOTDIR.'/../bin/q '.$set->QOPTIONS;
    $AUTHKEY  = "";
}

$SESSID  = md5(uniqid(rand(),true));
$set->TMPDIR = $TMPDIR;
$set->SESSID = $SESSID;
$stderr = "$TMPDIR/$SESSID.err";
$stdout = "$TMPDIR/$SESSID.out";

# Datapackage information
$DPKG_INFOS = array( 'URL_JSONDEF' => $set->URL_JSONDEF,
                     'DATAPACKAGE_VERSION' => $set->DATAPACKAGE_VERSION,
                     'LICENSE' => $set->LICENSE );


$sep = "\t";

?>
