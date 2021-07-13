<?php
/* ID pdf.inc v1.2
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

if ($_SERVER['PATH_INFO'])
do {

    $urloptions=array();
    $dataset='';
    $cntopt=0;

    # Parse PATH_INFO
    $urloptions = explode("/",$_SERVER['PATH_INFO']);
    array_shift($urloptions);
    $cntopt=count($urloptions);
    $dataset = array_shift($urloptions);

# Case of dataset not specify
    if (!strlen($dataset)) {
        break;
    }

    if (! is_dir($ENV['DATADIR']."/$dataset")) {
        echo "<pre>Error: No dataset $dataset</pre>";
        break;
    }

    # New instance of the def_lib object
    $def = new def_lib($ENV, $dataset);

    # check if authkey is valid for this dataset
    if ( $def->check_authorization($auth) )
    {
       echo $def->retmsg;
       break;
    }

    $cntopt--;
    $pdf_name = array_shift($urloptions);
    if ($cntopt<1 || strlen($pdf_name)==0 ) {
        echo "<pre>Error : PDF name required</pre>";
        break;
    }

    $filename = $ENV['DATADIR']."/$dataset/pdf/$pdf_name";
    if (! is_file($filename)) {
        echo "<pre>Error : PDF file $pdf_name.pdf does not exist</pre>";
        break;
    }

    // send the right headers
    header("Content-Type: application/pdf");
    header("Content-Length: " . filesize($filename));

    // Send the file to the browser.
    readfile($filename);

} while (0);



?>

