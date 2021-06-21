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

$GETDATA_URL_PROXY = getenv("GETDATA_URL_PROXY", true) ? getenv("GETDATA_URL_PROXY") : $set->GETDATA_URL_PROXY;

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

$EXTFILES = $set->EXTFILES;
$sep = "\t";

?>
