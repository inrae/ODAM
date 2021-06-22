<?php
/* ID build.inc v1.2
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

$debug = isset($_GET['debug']) ?  $_GET['debug'] : 0 ;

# Check format
if (! in_array( $format, array('cli', 'xml','json','tsv','txt','csv','ssv') ))
{
   echo "Error: format $format does not recognize.";
   exit(1);
}

if ($_SERVER['PATH_INFO'])
do
{
    $urloptions = explode("/",$_SERVER['PATH_INFO']);
    array_shift($urloptions);

  # count options
    $cntopt=count($urloptions);
    if (strlen(end($urloptions))== 0) $cntopt--;

  # check if debug
    if ($urloptions[$cntopt-1] == 'debug')
    {
        $cntopt--;
        $debug=1;
    }

  # check if dataset present
    $dataset = array_shift($urloptions);
    if (! is_dir("$DATADIR/$dataset"))
   {
        echo "ERROR: the dataset directory is not available";
        break;
    }

    ob_start();
    include ('inc/def_lib.inc');
    ob_end_clean();

  # New instance of the def_lib object
    $def = new def_lib("$DATADIR/$dataset");

  # Populate the '$def' instance with configuration variables coming from the '$set' instance (Set_Config object)
    $def->OS = $OS ;
    $def->TMPDIR = $TMPDIR ;
    $def->SESSID = $SESSID ;
    $def->EXTFILES = $EXTFILES;
    $def->SQLITE = $SQLITE;
    $def->GETDATA_URL_PROXY = $GETDATA_URL_PROXY ;

  # Build the database
    $time_start = microtime(true);
    $retval = $def->build_sqlite_db($debug);
    $time_end = microtime(true);
    $execution_time = round($time_end - $time_start,4);

  # Status
    $status = ($retval==0) ? 'Success' : 'Error' ;

  # Output if debug
    if ($debug) {
       if ($format == 'cli')
           echo "$retval\n";
       elseif ($format == 'json') {
          header('Content-Type: application/json; charset=ISO-8859-1');
          echo json_encode(explode("\n", $retval));
       }
       elseif ($format == 'xml') {
           `echo "statement\n$retval" > $stdout`;
           $def->echo_results($format);
       }
       else
           echo "<pre>$retval</pre>";
       break;
    }

  # Output - format cli
    if ($format == 'cli') {
       echo "$status - Execution time: $execution_time sec\n";
       if ($retval>0)
           echo trim(`cat $stderr`);
       break;
    }

  # Output - format xml,json, ...
    $headers = "Status\tExecution_time";
    $line = "$status\t$execution_time";
    if ($retval>0) {
       $headers .= "\tErrors";
       if ($OS=="LINUX")
           $line .= "\t".trim(`cat $stderr | sed -e "s/Error://" | tr "\n" ";" | tr -d "\r"`);
       else
           $line .= "\t".preg_replace('/Error:/', '', preg_replace("/[\n\r]/", ";", `cat $stderr`));
    }

    if ($OS=="LINUX")
       `echo "$headers\n$line" > $stdout`;
    else {
      `echo $headers > $stdout`;
      `echo $line >> $stdout`;
    }
    $def->echo_results($format);

} while (0);

`rm -f $TMPDIR/$SESSID.*`;

?>
