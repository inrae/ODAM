<?php
/* ID check.inc v1.2
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
include ('inc/check_lib.inc');
ob_end_clean();

#-----------------------------------------------------
# Carry out all the checklist
#-----------------------------------------------------
do {

    $stderr = "$TMPDIR/$SESSID.err";
    $xmlout = "$TMPDIR/$SESSID.xml";

    $urloptions=array();
    $dataset='';
    $cntopt=0;
    
    # Parse PATH_INFO if needed
    if ($_SERVER['PATH_INFO']) {
        $urloptions = explode("/",$_SERVER['PATH_INFO']);
        array_shift($urloptions);
        $cntopt=count($urloptions);
        $dataset = array_shift($urloptions);
    }

# Case of dataset not specify
    if (!strlen($dataset)) {
        $def = new check_lib('');
        break;
    }

# New instance of the def_lib object
    $def = new check_lib("$DATADIR/$dataset");

# Check dataset directory
    if ($def->check_dataset_dir()) break;

# Check if s_subsets and a_attributes files are available ?
    if ($def->check_def_files()) break;

# Check format of s_subsets and a_attributes files
    if ($def->check_def_format()) break;

# Check if subset names are consistent
    if ($def->check_subset_names()) break;

# Parse the definition files
    if ($def->build_hashDS ()) {
       echo $def->retmsg;
       break;
    }

# Check if data subset files are available ?
    if ($def->check_subset_files()) break;

# Check if  spelling of the attribute names are proper ?
    if ($def->check_attribute_spelling()) break;

# Check if attribute names are available ?
    if ($def->check_attribute_names()) break;

# Check if identifiers are consistent
    if ($def->check_identifiers()) break;

# Populate the '$def' instance (check_lib object) with configuration variables coming from the '$set' instance (Set_Config object)
#  => Environment and tool variables involved in the SQL querying of the TSV data subset files (cf do_SQL_query)
    $def->OS = $OS ;
    $def->TMPDIR = $TMPDIR ;
    $def->SESSID = $SESSID ;
    $def->EXTFILES = $EXTFILES;
    $def->QSQL   = $QSQL ;

# Check if the database is correctly built
    if ($def->OS == "LINUX")
       $def->check_SQL_db();

# Check if SQL on each subset file works
    if ($def->check_SQL_subsets(0)) break;

# Check if SQL on each merged subset file works
    if ($def->check_SQL_subsets(1)) break;

} while (0);


#-----------------------------------------------------
# Generate the checking results as output 
#-----------------------------------------------------
if ($format == 'xml') { # within a XML structure 
   header('Content-Type: text/xml; charset=ISO-8859-1');
   echo $def->check_xml_output("$GETDATA_URL_PROXY"."xsl/check_report.xsl");
}
elseif ($format == 'json') { # within a JSON structure
   header('Content-Type: application/json; charset=ISO-8859-1');
   echo $def->check_json_output();
}

`rm -f $TMPDIR/$SESSID.*`;

?>
