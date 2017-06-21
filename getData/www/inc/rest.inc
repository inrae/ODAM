<?php
/* ID rest.inc v1.0
 * Copyright (C) 2015 INRA - DJ
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
include ('inc/tools.inc');
ob_end_clean();

$auth = isset($_GET['auth']) ?  $_GET['auth'] : '' ;

# Parse PATH_INFO if needed
#if ($_SERVER['PATH_INFO'] && $auth==$AUTHKEY)
if ($_SERVER['PATH_INFO'])
do {

    $urloptions = explode("/",$_SERVER['PATH_INFO']);
    array_shift($urloptions);

    $cntopt=count($urloptions);

    $dataset = array_shift($urloptions);
    if (! is_dir("$DATADIR/$dataset")) {
        echo "ERROR: the dataset directory is not available";
        break;
    }
    $def = new def_lib("$DATADIR/$dataset");
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
    $DSNAME=$dataset;

#echo "<pre>"; print_r($def->hDataset); echo "</pre>"; break;

    $NB_FILES = 0;
    foreach ($def->FILES as $id => $file) {
         if (is_file("$DATADIR/$DSNAME/$file")) $NB_FILES++;
    }
    $IS_FILES = ($NB_FILES == count($def->FILES)) ? 1 : 0 ;

    $func = array_shift($urloptions);
    $restmsg="cnt=$cntopt;func=$func";

    $cntopt--;
    if ($cntopt<1 || strlen($func)==0 ) $func = 'subset';

    $doQuery = 0; $retval=0; $CMD='';


    #-------------------------
    # Get Subsets information
    #-------------------------
    if ($func == 'subset') {
        $list = $def->getSubsetList();
        $line = "Subset;Description;Identifier;WSEntry;SetID;LinkID;CV_Term_ID;CV_Term_Name\n";
        $line = implode($sep, array('Subset','Description','Identifier','WSEntry','SetID','LinkID','CV_Term_ID','CV_Term_Name'))."\n";
        foreach($list as $key => $arr) $line .= implode($sep,$arr)."\n";
        $fw=fopen($stdout,"w+");
        $fp=fwrite($fw,$line);
        fclose($fw);
    }
    #-------------------------
    # Get data from merging of some subsets (columns) ...
    #-------------------------
    elseif ( preg_match('/\(/',$func)==1 && preg_match('/\)/',$func)==1 ) {
        if ( preg_match('/^\((.+)\)/', $func, $matches) ) {
            $tarr_DS = (preg_match('/,/',$func)>0) ? explode(',', $matches[1]) : array( $matches[1] );
            $key=''; $value='';
            # ... all rows of the merged subsets
            if ($cntopt==1) {
                $query = $def->getSelectMulti($tarr_DS, $key,$value);
                $doQuery = 1;
            }
            elseif ($cntopt==2) {
                 $key = array_shift($urloptions);
                 # ... based on a category in the Category List =(factor|quantitative|qualitative|identifier)
                 if (in_array($key, $def->categories) ) {
                    $list = $def->getCategoryList($tarr_DS, $key);
                    $line = implode($sep, array('Subset','Attribute','Description','Type','CV_Term_ID','CV_Term_Name'))."\n";
                    foreach($list as $lkey => $arr) $line .= implode($sep,$arr)."\n";
                    $fw=fopen($stdout,"w");
                    $fp=fwrite($fw,$line);
                    fclose($fw);
                 }
                 if ($key=="entry") {
                    $list = $def->getEntryList($tarr_DS);
                    $line = implode($sep, array('Subset','Attribute','WSEntry'))."\n";
                    foreach($list as $key => $arr) $line .= implode($sep,$arr)."\n";
                    $fw=fopen($stdout,"w");
                    $fp=fwrite($fw,$line);
                    fclose($fw);
                 }
            }
            # ... based on a  value of a specific attribute 
            elseif ($cntopt==3) {
                $key = array_shift($urloptions);
                $value = array_shift($urloptions);
                $restmsg .=";$key=$value";
                $query = $def->getSelectMulti($tarr_DS, $key,$value);
                $doQuery = 1;
            }
        }
    }

    #-------------------------
    # Get data from one subset ...
    #-------------------------
    elseif (array_key_exists($func,$def->hDataset)) {
       if ($cntopt==1) {
            # ... all rows of the subset
           $query=$def->getSelect1($func,'','');
           $doQuery = 1;
       } else {
           $key = array_shift($urloptions);
           # ... based on an entry value 
           if (array_key_exists($key,$def->hDataset[$func]['entries']) && $cntopt==3) {
               $value = array_shift($urloptions);
               $query=$def->getSelect1($func,$key,$value);
               $restmsg .=";$key=$value";
               $doQuery = 1;
           }
           # ... based on a category in the Category List =(factor|quantitative|qualitative|identifier)
           elseif (array_key_exists($key,$def->hDataset[$func]['categories'])) {
               # get a list of a specific catagory ($key)
               if ($cntopt==2) {
               } else {
               }
           }
       }
    }

    # Nothing
    else break;

    # Launch query if needed
    $retval = $doQuery ?  doQuery($DSNAME, $query) : 0;

#echo "OK $restmsg - $cntopt - $CMD - $retval - $stdout<br>$query";
#exit();

    if ($retval) {
       $ERRORMSG='QUERY:&nbsp;'.$query.'<BR>===>&nbsp;'.`cat $stderr`.'<BR><BR>';
       echo $ERRORMSG;
    }
    else {
       echo_results($format);
    }

} while (0);

# Put infos in logfile
# if (!file_exists($logfile)) { `touch $logfile`; }
# $line = "$theIP @ $theURI @ $restmsg\n";
# $fw=fopen($logfile,"a+");
# $fp=fwrite($fw,$line);
# fclose($fw);

`rm -f $TMPDIR/$SESSID.*`;


?>
