<?php
/* ID config.inc v1.0
 * Copyright (C) 2015 INRA - DJ

 */

include_once ('inc/Set_Config.inc');
$set = new Set_Config(array('-conf'=>"etc/getdata.conf",'-global'=>FALSE));

#----------------------------------------------
# Populate some variables 
#----------------------------------------------

$GETDATA_URL_PROXY = isset($_ENV["GETDATA_URL_PROXY"]) ? $_ENV["GETDATA_URL_PROXY"] : $set->GETDATA_URL_PROXY;
$ROOTDIR  = $set->ROOTDIR;

$TMPDIR  = "$ROOTDIR/tmp";
$SESSID  = md5(uniqid(rand(),true));
$set->TMPDIR = $TMPDIR;
$set->SESSID = $SESSID;

$DATADIR  = $set->DATAROOT;
$AUTHKEY  = $set->AUTHKEY;

$QBIN     = $set->QBIN;
$QOPTIONS = $set->QOPTIONS;


# DEBUG status
$DEBUG          = $set->DEBUG;


?>
