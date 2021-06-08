<?php
/* ID rest.inc v1.2
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

$auth = isset($_GET['auth']) ?  $_GET['auth'] : '' ;
$debug = isset($_GET['debug']) ?  $_GET['debug'] : 0 ;
$limit = isset($_GET['limit']) ?  $_GET['limit'] : 0 ;
$links = isset($_GET['links']) ?  $_GET['links'] : 0 ;
$restmsg = '';
$metadata=0;
$datapackage=0;


# Check format
if (! in_array( $format, array('xml','json','tsv') )) {
   echo "Error: format $format does not recognize.";
   exit(1);
}

# Check limit
if (is_numeric($limit))
   $limit = $limit + 0;
else
   $limit = 0;

# Parse PATH_INFO if needed
#if ($_SERVER['PATH_INFO'] && $auth==$AUTHKEY)
if ($_SERVER['PATH_INFO'])
do
{
    $urloptions = explode("/",$_SERVER['PATH_INFO']);
    array_shift($urloptions);

    # count options
    $cntopt=count($urloptions);
    if (strlen(end($urloptions))== 0) $cntopt--;

    # check if debug
    if ($urloptions[$cntopt-1] == 'debug') {
        $cntopt--;
        $debug=1;
    }

    # check if metadata|attribute
    if (in_array($urloptions[$cntopt-1], array('metadata','attribute'))) {
        $cntopt--;
        $metadata=1;
    }
    
    # check if datapackage
    if (in_array($urloptions[$cntopt-1], array('datapackage'))) {
        $cntopt--;
        $datapackage=1;
    }

    # check if dataset present
    $dataset = array_shift($urloptions);
    if (! is_dir("$DATADIR/$dataset")) {
        echo "ERROR: the dataset directory is not available";
        break;
    }

    # get function
    $func = array_shift($urloptions);

    $cntopt--;
    if ($cntopt<1 || strlen($func)==0 ) $func = 'subset';
    $restmsg = "cntopt=$cntopt; func=$func";

    $doQuery = 0; $retval=0;

    # if check function
    if ($func == 'check') {
        include ('inc/check.inc');
        break;
    }
    
    ob_start();
    include ('inc/def_lib.inc');
    ob_end_clean();

    # New instance of the def_lib object
    $def = new def_lib("$DATADIR/$dataset");

# Populate the '$def' instance with configuration variables coming from the '$set' instance (Set_Config object)
#  => Environment and tool variables involved in the SQL querying of the TSV data subset files (cf do_SQL_query)
    $def->OS = $OS ;
    $def->TMPDIR = $TMPDIR ;
    $def->SESSID = $SESSID ;
    $def->QSQL   = $QSQL ;
    $def->GETDATA_URL_PROXY = $GETDATA_URL_PROXY ;

    # check if metadata files
    if ( $def->check_deffile('s_subsets') ) {
       echo 's_subsets: '.$def->retmsg;
       break;
    }
    if ( $def->check_deffile('a_attributes') ) {
       echo 'a_attributes: '.$def->retmsg;
       break;
    }
    if ($def->build_hashDS () != 0) {
       echo $def->retmsg;
       break;
    }
#echo "<pre>"; print_r($def->hDataset); echo "</pre>"; break;

    $DSNAME=$dataset;
    $NB_FILES = 0;
    foreach ($def->FILES as $id => $file) {
         if (is_file("$DATADIR/$DSNAME/$file")) $NB_FILES++;
    }
    $IS_FILES = ($NB_FILES == count($def->FILES)) ? 1 : 0 ;

    #-----------------------------------------------------
    # Inner block : parses parameters and builds the query
    #-----------------------------------------------------
    do {
        $MSG ='';

        # Get Subsets information
        if ($func == 'subset') {
            $list = $def->getSubsetList();
            $line = "Subset;Description;Identifier;WSEntry;SetID;LinkID;CV_Term_ID;CV_Term_Name\n";
            $line = implode($sep, array('Subset','Description','Identifier','WSEntry','SetID','LinkID','CV_Term_ID','CV_Term_Name'))."\n";
            foreach($list as $key => $arr) $line .= implode($sep,$arr)."\n";
            $fw=fopen($stdout,"w+");
            $fp=fwrite($fw,$line);
            fclose($fw);
            break;
        }

        # Get data from merging of some subsets (columns)
        if ( preg_match('/^\((.+)\)/', $func, $matches) ) {
            $tarr_DS = (preg_match('/,/',$func)>0) ? explode(',', $matches[1]) : array( $matches[1] );
            $key=''; $value=''; $op='';
            # Get list of entries
            $arr_entry = array();
            foreach ($tarr_DS as $ds) {
                if (! array_key_exists($ds,$def->hDataset) ) {
                    $MSG = "ERROR: $ds is not a valid data subset";
                    break;
                }
                $list = $def->getEntryList(array($ds), 1);
                foreach($list as $var => $arr) array_push($arr_entry, $arr[2]);
            }
            if (strlen($MSG)>0) break;
            $arr_entry = array_unique($arr_entry);
            $queryMulti=1;
        }
        # Get data from only one subset
        elseif (array_key_exists($func,$def->hDataset)) {
            $arr_entry = array();
            $list = $def->getEntryList(array($func), 0);
            foreach($list as $var => $arr) array_push($arr_entry, $arr[2]);
            $queryMulti=0;
        }
        else {
            $MSG = "Error: $func is not a valid data subset";
            break;
        }
    
        # all rows of the (merged) subset(s)
        if ($cntopt==1) {
            $query = $queryMulti ? $def->getSelectMulti($tarr_DS, $key,$value,$op) : 
                                   $def->getSelect1($func,$key,$value,$op);
            $doQuery = 1;
            break;
        }

        # Get the category / entry list
        if ($cntopt==2) {
            $key = array_shift($urloptions);
            # a category in the Category List =(factor|quantitative|qualitative|identifier)
            if (in_array($key, $def->categories) ) {
                $list = $queryMulti ? $def->getCategoryList($tarr_DS, $key) :
                                      $def->getCategoryList(array($func), $key, 0);
                $line = implode($sep, array('Subset','Attribute','Description','Type','CV_Term_ID','CV_Term_Name'))."\n";
                foreach($list as $lkey => $arr) $line .= implode($sep,$arr)."\n";
                $fw=fopen($stdout,"w");
                $fp=fwrite($fw,$line);
                fclose($fw);
            }
            # entry/alias list
            elseif ($key=="entry" || $key=="alias") {
                $line = implode($sep, array('Subset','Attribute','WSEntry'))."\n";
                if ($queryMulti) {
                    foreach ($tarr_DS as $ds) {
                        $list = $def->getEntryList(array($ds), 1);
                        foreach($list as $key => $arr) $line .= implode($sep,$arr)."\n";
                    }
                } else {
                    $list = $def->getEntryList(array($func), 0);
                    $line = implode($sep, array('Subset','Attribute','WSEntry'))."\n";
                    foreach($list as $key => $arr) $line .= implode($sep,$arr)."\n";
                }
                $line = implode("\n",array_unique(explode("\n", $line)));
                $fw=fopen($stdout,"w");
                $fp=fwrite($fw,$line);
                fclose($fw);
            }
            else {
                $MSG = "Error: either $key is not a valid keyword or requires a value";
            }
            break;
        }

        # Get data based a value of a specific attribute 
        if ($cntopt==3) {
            $key = array_shift($urloptions);
            $value = array_shift($urloptions);
            if (in_array($key, $arr_entry)) {
                if ( strpos($value,'>') !== false || strpos($value,'<') !== false ) {
                    $op = substr($value, 0, 1);
                    $value = substr($value, 1);
                }
                $restmsg .="; $key$op=$value";
                $query = $queryMulti ? $def->getSelectMulti($tarr_DS, $key,$value,$op) : 
                                       $def->getSelect1($func,$key,$value,$op);
                $doQuery = 1;
            }
            else {
                $MSG = "Error: the entry $key is not a valid entry";
            }
            break;
       }

    } while(0);

    # If ERROR, display message then exit
    if (strlen($MSG)>0) {
        echo $MSG;
        break;
    }

    # if limit is specified
    if ($limit>0) $query .= " LIMIT $limit";

    # if debug, display information then exit
    if ($debug) {
        if ($OS=='LINUX' && $doQuery)
           echo "<pre>".`$QSQL -v 2>&1  | grep -v Python  | tr "\n" " - "`."</pre><hr>";
        $CMD = $doQuery ?  $def->do_SQL_query($query, '', 1) : 0;
        echo "URL QUERY : $restmsg<br><br>CMD = $CMD<br>"; echo "<br>STDOUT = $stdout<br>";
        if ($doQuery) {
         # Execute the query and return some information
           $time_start = microtime(true);
           $retvald = $def->do_SQL_query($query);
           $time_end = microtime(true);
           $execution_time = $time_end - $time_start;
           $rescnt=0;
           $retvald=trim(`ls -l $stderr | cut -d' ' -f5`);
           if (! $retvald) $rescnt=trim(`cat $stdout | wc -l`);
           $rescnt--;
           echo "<br>doQuery: $doQuery - Execution time: $execution_time - Nb records: $rescnt<br>";
           if ($retvald) echo "<br>".`cat $stderr`."<br>";
         # display the formated query
           $strquery = $query;
           $query = str_replace("JOIN", "\nJOIN", str_replace("from", "\nFROM", $query));
           $query = str_replace("select", "SELECT", str_replace("where", "\nWHERE", $query));
           $nbds = count($def->hDataset);
           for ($i=1; $i<$nbds; $i++) {
              $j=$i+1; $ret=0;
              while($j<=$nbds) {
                 $match = 'f'.$i.'.[^,]+, f'.$j.'.';
                 $ret = preg_match("/$match/", $query, $str_match);
                 if ($ret) break;
                 $j++;
              }
              if ($ret) {
                 $str_split = preg_split("/,/", $str_match[0]);
                 $query = str_replace($str_split[0].',', $str_split[0].",\n      ", $query);
              }
           }
           echo"<br><pre>$query</pre>";
         # Analyse query
           $retvald = $def->do_SQL_query($strquery, '-A', 0);
           if (! $retvald) echo "<pre>".trim(`cat $stdout`)."</pre>";
        } else {
         # Display dataset structure
           echo "<hr>";
           echo "<pre>"; print_r($def->hDataset); echo "</pre>";
        }
        break;
    }

    # Get metadata
    if ($metadata==1) {
        if ($format == 'json') {
           header('Content-Type: application/json; charset=ISO-8859-1');
           echo json_encode($def->hDataset);
        } else {
           $def->echo_results($format, $file=$def->attrs_file);
        }
        break;
    }

    # Get metadata into a data-package schema
    if ($datapackage==1 && $format == 'json') {
        header('Content-Type: application/json; charset=ISO-8859-1');
        $urlroot = $links ? $URLSERVER : '';
        $def->DPKG_INFOS = $DPKG_INFOS;
        echo $def->getJSONDataTable($urlroot);
        break;
    }

    # Launch query if needed
    $retval = $doQuery ?  $def->do_SQL_query($query) : 0;
    if ($retval) {
        $ERRORMSG='QUERY:&nbsp;'.$query.'<BR>===>&nbsp;'.`cat $stderr`.'<BR><BR>';
        echo $ERRORMSG;
        break;
    }

    # Output the results
    $def->echo_results($format);

} while (0);

`rm -f $TMPDIR/$SESSID.*`;

?>
