<?php
/* ID def_lib_ext.inc v1.0
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
include('def_lib.inc');
ob_end_clean();

class def_lib_ext extends def_lib {

    /* environment and tool variables involved in the SQL querying of the CSV data subset files (cf do_SQL_query-) */
    var $TMPDIR    = "";
    var $SESSID    = "";
    var $QSQL      =  "";
    var $error_msg = "";

    var $check_flag = 1;

    var $status_tag = array( 'ok' => 'ok', 'bad' => 'Error');

    var $ds_items = array( 'directory'    => array( 'msg' => 'the dataset directory', 
                                            'info'=>'', 'status' => 'not checked', 'cat'=>'General' ),
                   'subsetdef'    => array( 'msg' => 'the subset definition file',
                                            'info'=>'', 'status' => 'not checked', 'cat'=>'General' ),
                   'attributedef' => array( 'msg' => 'the attribute definition file',
                                            'info'=>'', 'status' => 'not checked', 'cat'=>'General' ),
                   'subsetfiles'  => array( 'msg' => 'data subset files',
                                            'info'=>'','status' => 'not checked', 'cat'=>'General' ),
                   'subset_def_format'    => array( 'msg' => 'definition file format',
                                            'info'=>'','status' => 'not checked', 'cat'=>'s_subsets' ),
                   'attribute_def_format' => array( 'msg' => 'definition file format',
                                            'info'=>'','status' => 'not checked', 'cat'=>'a_attributes' )
    );

    var $ref_notes = array(
         1 => "A directory named as the dataset name should be actually created in the data repository; \\n Be careful in the spelling, see note 6;",
         2 => "The s_subsets.tsv and a_attributes.tsv files should be present in the data repository",
         3 => "All data subset files declared in the s_subsets.tsv (col 5) should be available in the data repository",
         5 => "To be sure to have the right format, do a 'copy' of data from the spreadsheet then 'paste' them into a new file, then 'save as TSV format (separator: a tab character)'",
         4 => "1) all subsets in the a_attributes.tsv file (col 1) should be declared in the s_subsets.tsv file (col 3)\\n 2) all subsets in the s_subsets.tsv file (col 3) should be declared in the a_attributes.tsv file (col 1)\\n 3) all attribute names in the a_attributes.tsv file (col 2) should be available as a column in the corresponding data subset file declared in the s_subsets.tsv file (col 5)",
         6 => "Be careful in the spelling: \\n 1) for data subset file names (col 5 in s_subsets.tsv), identifier name (col 2 in s_subsets.tsv), attribute names (col 2 in a_attributes.tsv), subset shortnames (col 3 in s_subsets.tsv and col 1 in a_attributes.tsv) and entry names (col 3 in a_attributes.tsv), only the alphanumerical characters and the underscore are allowed (i.e 'a-z', 'A-Z', '0-9' and '_'). Moreover, these names should not start with a digit!\\n 2) for categorical names (col 4 in a_attributes.tsv), the number of terms and their spelling are fixed, namely: 'identifier','factor','quantitative','qualitative'. \\n 3) for type (col 5 in a_attributes.tsv), the allowed names are restrited to 'numeric' or 'string'. \\n 4) for descritpion, the allowed characters are : 0-9 a-z A-Z , : + * () [] {} - % ! | / . ?",
         7=> "Identifiers declared in the s_subsets.tsv file (col 2)\\n 1) should be declared as 'identifier' in the 'category' column in the a_attributes.tsv file (col 4) \\n 2) should be available as an column item in the corresponding data subset file \\n 3) should be the only one attribute declared as identifier for the corresponding data subset file in the a_attributes.tsv file (col 4)",
         8=> "Each subset having a 'father_rank' greater than 0 in the s_subsets.tsv file (col 2)\\n 1) should  have a column label in the corresponding data file that must be identical to the identifier label of the linked subset (i.e. corresponding to the 'father_rank' in col 1) \\n 2) should have the linked subset identifier with no category (i.e. void) in the a-attributes.tsv file (col 4), except if the subset and the linked subset have the same identifier"
    );

/********************************************************
* Init. Method / Constructor
********************************************************/
    function __construct($input) { $this->def_lib_ext($input); } # PHP 5.3-compliant

    # Init. Vars.
    function def_lib_ext($input) {
        parent::__construct($input);
        if (!strlen($input)) $this->check_flag=0;
    }

# Check dataset directory
    function check_dataset_dir() {
        $ret=0;
        do {
            if (! is_dir($this->dir_input)) {
               $this->ds_items['directory']['info'] = "Not available; See note (1)";
               $this->ds_items['directory']['status'] = $this->status_tag['bad'];
               $this->ref_notes[$this->idx_refnotes++] = $this->list_notes[1];
               $ret=1;
               break;
            }
            $this->ds_items['directory']['info'] = "Available";
            $this->ds_items['directory']['status'] = $this->status_tag['ok'];
        } while(0);
        return $ret;
    }

# Check if s_subsets and a_attributes files are available
    function check_def_files() {
        $ret=0;
        $this->ds_items['subsetdef']['info'] = is_file($this->dir_input."/s_subsets.tsv") ? "Available" : "Not available. See note (2)" ;
        $this->ds_items['subsetdef']['status'] = is_file($this->dir_input."/s_subsets.tsv") ? $this->status_tag['ok'] : $this->status_tag['bad'] ;
        $this->ds_items['attributedef']['info'] = is_file($this->dir_input."/a_attributes.tsv") ? "Available" : "Not available. See note (2)" ;
        $this->ds_items['attributedef']['status'] = is_file($this->dir_input."/a_attributes.tsv") ? $this->status_tag['ok'] : $this->status_tag['bad'] ;
        if ($this->ds_items['attributedef']['status'] == $this->status_tag['bad'] || $this->ds_items['subsetdef']['status'] == $this->status_tag['bad'])
            $ret=1;
        return $ret;
    }

# Check format of s_subsets and a_attributes files
    function check_def_format() {
        $ret=0;
    # Check format of s_subsets file
        $ret1 = $this->check_deffile('s_subsets');
        $this->ds_items['subset_def_format']['info'] = $ret1==0 ? 'CSV-compliant' : $this->retmsg." See note (5)" ;
        $this->ds_items['subset_def_format']['status'] = $ret1==0 ? $this->status_tag['ok'] : $this->status_tag['bad'] ;
    
    # Check format of a_attributes file
        $ret2=$this->check_deffile('a_attributes');
        $this->ds_items['attribute_def_format']['info'] = $ret2==0 ? 'CSV-compliant' : $this->retmsg." See note (5)" ;
        $this->ds_items['attribute_def_format']['status'] = $ret2==0 ? $this->status_tag['ok'] : $this->status_tag['bad'] ;
    
        if ($ret1 || $ret2) $ret=1;

        return $ret;
    }

# Check if data subset files are available
    function check_subset_files() {
        $ret=0;
        $NB_FILES = 0;
        $fileinfo ='';
        foreach ($this->FILES as $id => $file) {
             if (is_file($this->dir_input."/$file")) { $NB_FILES++; }
             else $fileinfo .="$file is missing, See note (3)\\n";
        }
        $IS_FILES = ($NB_FILES == count($this->FILES)) ? 1 : 0 ;
        $this->ds_items['subsetfiles']['info'] = $IS_FILES ? "all availables (".$NB_FILES.")" : $fileinfo ;
        $this->ds_items['subsetfiles']['status'] = $IS_FILES ? $this->status_tag['ok'] : $this->status_tag['bad'] ;
        if (! $IS_FILES) $ret=1;
        return $ret;
    }

# Check if  spelling of the attribute names are proper ?
    function check_attribute_spelling() {
        $ret=0;
        $ret = $this->check_attributes();
        $msg = "Check if spelling of names are proper in a_attributes.tsv";
        $info = $ret==0 ? "all attributes seem ok" : $this->retmsg ;
        $status = $ret==0 ? $this->status_tag['ok'] : $this->status_tag['bad'] ;
        $this->ds_items["attrspelling"] = array( 'msg' => $msg, 'info' => $info, 'status' => $status, 'cat' => "a_attributes" );
        return $ret;
    }

# Check if attribute names are available ?
    function check_attribute_names() {
        $ret=0;
        foreach ($this->FILES as $id => $file) {
             $ret = $ret || $this->check_subset($file);
             $msg = "Check if column names are the same that those declared in a_attributes.tsv";
             $info = $ret==0 ? "all attributes seem ok" : $this->retmsg ;
             $status = $ret==0 ? $this->status_tag['ok'] : $this->status_tag['bad'] ;
             $this->ds_items["f$id"] = array( 'msg' => $msg, 'info' => $info, 'status' => $status, 'cat' => "$file" );
        }
        return $ret;
    }

# Check if subset names are consistent between both "s_subsets" and "a_attributes" files
    function check_subset_names() {
        $ret=0;
        $f0=fopen($this->subsets_file,"r");
        $arr_subsets = array();
        while ( ($data = fgetcsv($f0,0,"\t") ) !== FALSE ) {
             array_push($arr_subsets, $data[2]);
        }
        #fclose($fo);
        $arr_msg = array();
        $f1=fopen($this->attrs_file,"r");
        $subset="subset";
        while ( ($data = fgetcsv($f1,0,"\t") ) !== FALSE ) {
             if ($data[0] == $subset) continue;
             $subset=$data[0];
             if ( ! in_array($subset, $arr_subsets) ) {
                 array_push($arr_msg, "the shortname '$subset' seems not to be declared in the s_subsets.tsv file., See note (4.1)");
                 $ret=1;
             }
        }
        fclose($f1);
        $msg = 'Check if subset names are consistent';
        if ($ret) $this->retmsg = implode( '\\n', $arr_msg );
        $info = $ret==0 ? "all subset names seem consistent" : $this->retmsg ;
        $status = $ret==0 ? $this->status_tag['ok'] : $this->status_tag['bad'] ;
        $this->ds_items['subset names'] = array( 'msg' => $msg, 'info' => $info, 'status' => $status, 'cat' => 'subset names' );

        return $ret;
    }

# Check if identifiers are consistent
    function check_identifiers() {
        $ret=0;
        $f0=fopen($this->subsets_file,"r");
        $nb_s=-1;
        $arr_subsets = array();
        $arr_attributes = array();
        while ( ($data = fgetcsv($f0,0,"\t") ) !== FALSE ) {
             $nb_s++; if ($nb_s==0) continue;
             $arr_subsets[$data[0]] = $data;
             $f1=fopen($this->dir_input.'/'.$data[4],"r");
             $arr_attributes[$data[0]] = fgetcsv($f1,0,"\t");
             fclose($f1);
        }
        #fclose($fo);
        $arr_msg = array();
        foreach ($arr_subsets as $key => $arr) {
             if (  count($this->hDataset[$arr[2]]['categories']['identifier'])>1 ) {
                array_push($arr_msg, "'".$arr[3]."' is inconsistent in ".$arr[4].", See note (7.3)");
                $ret=1;
             }
             if (  $this->hDataset[$arr[2]]['categories']['identifier'][0] !== $arr[3] ) {
                array_push($arr_msg, "'".$arr[3]."' is inconsistent in ".$arr[4].", See note (7.1)");
                $ret=1;
             }
             if (! in_array($arr[3], $arr_attributes[$arr[0]] ) ) {
                array_push($arr_msg, "'".$arr[3]."' is inconsistent in ".$arr[4].", See note (7.2)");
                $ret=1;
             }
             if ($arr[1]>0 && ! in_array($arr_subsets[$arr[1]][3], $arr_attributes[$arr[0]] ) ) {
                array_push($arr_msg, "'".$arr_subsets[$arr[1]][3]."' is inconsistent in ".$arr[4].", See note (8.1)");
                $ret=1;
             }
             foreach ($this->hDataset[$arr[2]]['categories'] as $key => $arr_cat) {
                 if (in_array($arr_subsets[$arr[1]][3], $arr_cat) && $key !== 'identifier' ) {
                     array_push($arr_msg, "'".$arr_subsets[$arr[1]][3]."' is inconsistent in ".$arr[4].", See note (8.2)");
                     $ret=1;
                     break;
                 }
             }

        }
        $msg = 'Check if identifiers are consistent';
        if ($ret) $this->retmsg = implode( '\\n', $arr_msg );
        $info = $ret==0 ? "all identifiers seem consistent" : $this->retmsg ;
        $status = $ret==0 ? $this->status_tag['ok'] : $this->status_tag['bad'] ;
        $this->ds_items['identifiers'] = array( 'msg' => $msg, 'info' => $info, 'status' => $status, 'cat' => 'identifiers' );

        return $ret;
    }

/*
# populate_vars_conf: populate this instances with variables of another object (Set_Config)
    function populate_vars_conf($object) {
        foreach ( get_object_vars($object) as $var => $val) $this->$var=$val;
    }
*/

# do SQL query 
    function do_SQL_query($query) {
        $stderr = $this->TMPDIR.'/'.$this->SESSID.'.err';
        $stdout = $this->TMPDIR.'/'.$this->SESSID.'.out';
        $QSQL = $this->QSQL;
        $CMD = "(cd ".$this->dir_input."; $QSQL \"$query\" | uniq)";
        $retval=trim(`$CMD 1>$stdout 2>$stderr; ls -l $stderr | cut -d' ' -f5`);
        if ($retval) {
           $CMD = "(cd ".$this->dir_input."; cat $stderr |  sed -e \"s|.*Error|Error|\")";
           $this->error_msg = trim(`$CMD`);
        }
        return $retval;
    }

# Check if SQL on each subset file works
    function check_SQL_subsets($merge) {
        $arr_msg = array();
        $arr = array_keys($this->hDataset);
        array_pop($arr);
        $ret=0;
        foreach ($arr as $subset) {
              $query = $merge==0 ? $this->getSelect1($subset,'','') : $this->getSelectMulti(array($subset),'','') ;
              $qstr  = $merge==0 ? "'$subset'" : "($subset)";
              if ($this->do_SQL_query($query)) {
                  array_push($arr_msg, "SQL query on $qstr subset fails: ".$this->error_msg);
                  $ret=1;
              }
        }
        $key = $merge==0 ? 'sqlsubsets' : 'sqlmergedsubsets' ;
        $msg = $merge==0 ? 'Check SQL on each subset' : 'Check SQL on each merged subset';
        $cat = $merge==0 ? 'subsets' : 'merged_subsets';
        if ($ret) $this->retmsg = implode( ' - ', $arr_msg );
        $info = $ret==0 ? "all SQL success" : $this->retmsg ;
        $status = $ret==0 ? $this->status_tag['ok'] : $this->status_tag['bad'] ;
        $this->ds_items[$key] = array( 'msg' => $msg, 'info' => $info, 'status' => $status, 'cat' => $cat );

        return $ret;
    }

# Count the number of errors
    function check_count_errors() {
        $nberrors=0;
        foreach ($this->ds_items as $key => $arr)
           if ($arr['status']==$status_tag['bad']) $nberrors++;
        return $nberrors;
    }

# Generate the checking results within a XML structure
    function check_xml_output($xsl_url_file) {

        $nbrows=0;
        $nberrors=0;
        $ds_checks="";
        if ($this->check_flag) {
           $nbrows=count($this->ds_items);
           foreach ($this->ds_items as $key => $arr) {
              $ds_checks .= "<Row id=\"$key\" status=\"".$arr['status']."\">";
              $ds_checks .= "<Type>".$arr['cat']."</Type>";
              $ds_checks .= "<Description>".$arr['msg']."</Description>";
              $ds_checks .= "<Information>".$arr['info']."</Information>";
              $ds_checks .= "</Row>";
              if ($arr['status']==$this->status_tag['bad']) $nberrors++;
           }
        }

        $nbnotes = count($this->ref_notes);
        $chk_notes="";
        if ($nbnotes>0)
            foreach ($this->ref_notes as $key => $val) $chk_notes .= "<Note id=\"$key\">$val</Note>";

        $xml_output  = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n<?xml-stylesheet type=\"text/xsl\" href=\"$xsl_url_file\"?>\n";
        $xml_output .= "<WSCheck><Rows count=\"$nbrows\" errors=\"$nberrors\">$ds_checks</Rows><Notes count=\"$nbnotes\">$chk_notes</Notes></WSCheck>\n";
        
        return $xml_output;
    }

}

?>