<?php
/* ID infos.inc v1.2
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

ob_start();
include ('inc/def_lib.inc');
ob_end_clean();

do {

    $urloptions=array();
    $dataset='';
    $cntopt=0;

    $INFOS_TEXT = '';


    # Parse PATH_INFO if needed
    if ($_SERVER['PATH_INFO']) {
        $urloptions = explode("/",$_SERVER['PATH_INFO']);
        array_shift($urloptions);
        $cntopt=count($urloptions);
        $dataset = array_shift($urloptions);
    }

# Case of dataset not specify
    if (!strlen($dataset)) {
        $INFOS_TEXT = '## ERROR: no dataset provided';
        break;
    }

    if (! is_dir($ENV['DATADIR']."/$dataset")) {
        $INFOS_TEXT = "## ERROR: dataset $dataset is not available";
        break;
    }

    # New instance of the def_lib object
    $def = new def_lib($ENV, $dataset);

    # check if authkey is valid for this dataset
    if ( $def->check_authorization($auth) )
    {
       $INFOS_TEXT = '## '.$def->retmsg;
       break;
    }

    if (! is_file($ENV['DATADIR']."/$dataset/infos.md")) {
        if (is_file($ENV['DATADIR']."/$dataset/collection.txt")) {
             $INFOS_TEXT = '## No collection information';
        } else {
             $INFOS_TEXT = '## No dataset information';
        }
        break;
    }

    $INFOS_TEXT = file_get_contents($ENV['DATADIR']."/$dataset/infos.md");
    $INFOS_TEXT = preg_replace('/@@IMAGE@@/', $GETDATA_URL_PROXY."image/$dataset", $INFOS_TEXT);
    $INFOS_TEXT = preg_replace('/@@PDF@@/', $GETDATA_URL_PROXY."pdf/$dataset", $INFOS_TEXT);

} while (0);

#header('Content-Type: text/markdown; charset=ISO-8859-1');
header('Content-Type: text/markdown; charset=UTF-8');
echo $INFOS_TEXT;


?>

