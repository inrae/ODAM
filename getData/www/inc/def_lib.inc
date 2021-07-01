<?php
/* ID def_lib.inc v1.2
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

class def_lib
{
     var $categories   = array('identifier','factor','quantitative','qualitative');
     var $attrtypes    = array('numeric', 'string');
     var $def_infos    = array(
        's_subsets' => array(
           1=> array('name'=>'rank',         'type'=>'numeric'),
           2=> array('name'=>'father_rank',  'type'=>'numeric'),
           3=> array('name'=>'subset',       'type'=>'string'),
           4=> array('name'=>'identifier',   'type'=>'string'),
           5=> array('name'=>'file',         'type'=>'string'),
           6=> array('name'=>'description',  'type'=>'string'),
           7=> array('name'=>'CV_term_id',   'type'=>'string'),
           8=> array('name'=>'CV_term_name', 'type'=>'string') ),
        'a_attributes' => array(
           1=> array('name'=>'subset',       'type'=>'string'),
           2=> array('name'=>'attribute',    'type'=>'string'),
           3=> array('name'=>'entry',        'type'=>'string'),
           4=> array('name'=>'category',     'type'=>'string'),
           5=> array('name'=>'type',         'type'=>'string'),
           6=> array('name'=>'description',  'type'=>'string'),
           7=> array('name'=>'CV_term_id',   'type'=>'string'),
           8=> array('name'=>'CV_term_name', 'type'=>'string') ) );
     var $FILES        = array ();
     var $hDataset     = array();
     var $dsname       = "";
     var $dir_input    = "";
     var $subsets_file = "";
     var $attrs_file   = "";
     var $retmsg       = "OK";
     var $ret          = 0;
     var $FIELD_LIMIT  = -1;
     var $GETDATA_URL_PROXY = "http://localhost/getdata/";
     var $DPKG_INFOS = array( 'URL_JSONDEF' => 'https://inrae.github.io/ODAM/json-schema/',
                              'DATAPACKAGE_VERSION' => '1.0',
                              'LICENSE' => 'ODC-BY-1.0,https://opendatacommons.org/licenses/by/,Open Data Commons Attribution License 1.0' );

    /* environment and tool variables involved in the SQL querying of the TXT data subset files (cf do_SQL_query) */
     var $OS        = "LINUX";
     var $SQLITE    = "/usr/bin/sqlite3";
     var $SQLOPT    = "-tabs";
     var $DBNAME    = "sqlite3.db";
     var $TRANSAC   = 0;
     var $QSQL      =  "/usr/bin/q -H -O -t  -e iso-8859-1";
     var $EXTFILES  = "txt";
     var $TMPDIR    = "";
     var $SESSID    = "";
     var $error_msg = "";

/********************************************************
* Init. Method / Constructor
********************************************************/
    function __construct($input) { $this->def_lib($input); } # PHP 5.3-compliant

    # Init. Vars.
    function def_lib($input)
    {
        $this->dir_input = $input;
        $this->dsname = basename($this->dir_input);
        $this->ret=0;
        $this->subsets_file ="$input/s_subsets.tsv";
        $this->attrs_file ="$input/a_attributes.tsv";
    }

/********************************************************
* check_deffile
********************************************************/
    function check_deffile ($defname)
    {
        $ret=0;
        $this->retmsg = '';
        $tsvfile=$this->dir_input.'/'.$defname.'.tsv';
        $arr_col = $this->def_infos[$defname];
        $nbcol = count($arr_col);
        do {
            # files exist ?
            if (! is_file("$tsvfile")) {
               $this->retmsg = "the file is missing";
               $ret=1;
               break;
            }
            $fo=fopen($tsvfile,"r");
            if ( ($data = fgetcsv($fo,0,"\t") ) == FALSE) {
               $this->retmsg = "the file does not seem to be compliant with the TSV standard";
               $ret=1;
               break;
            }
            if ( count($data)<3) {
               $this->retmsg = "the separator seems not to be a tab character";
               $ret=1;
               break;
            }
            if ( count($data) != $nbcol) {
               $this->retmsg = "the column count is not the good one (must be equal to (".$nbcol.")";
               $ret=1;
               break;
            }
            $arr_msg = array();
            for ($i = 1; $i <= $nbcol; $i++ ) {
                if ( $data[$i-1] != $arr_col[$i]['name'] ) { array_push($arr_msg, "Col $i: name must be '".$arr_col[$i]['name']."'"); $ret=1; }
            }
            if ($ret) {
                $this->retmsg = implode( ' - ', $arr_msg );
                break;
            }
            $j=2;
            while ( ($data = fgetcsv($fo,0,"\t") ) !== FALSE ) {
                for ($i = 1; $i <= $nbcol; $i++ ) {
                    if ( $arr_col[$i]['type'] == 'numeric' && ! is_numeric($data[$i-1]) ) {
                         array_push($arr_msg, "Col '".$arr_col[$i]['name']."', Line $j: the value must be a numeric"); $ret=1;
                         $ret=1;
                    }
                    if ( $arr_col[$i]['type'] == 'url' && ( ! is_string($data[$i-1]) || ! preg_match('/^http:\/\//', $data[$i-1]) ) ) {
                         array_push($arr_msg, "Col '".$arr_col[$i]['name']."', Line $j: the value must be a URL"); $ret=1;
                         $ret=1;
                    }
                    if ( $arr_col[$i]['type'] == 'string' && ! is_string($data[$i-1]) ) {
                         array_push($arr_msg, "Col '".$arr_col[$i]['name']."', Line $j: the value must be a string"); $ret=1;
                         $ret=1;
                    }
                    if ( $arr_col[$i]['name'] == 'subset' && ! preg_match("/^[a-zA-Z][a-zA-Z0-9_]+$/", $data[$i-1])) {
                         array_push($arr_msg, "subste '".$data[$i-1]."' should follow the naming convention described in note (6.1)");
                         $ret=1;
                    }
                }
                $j=$j+1;
            }
            if ($ret) {
                $this->retmsg = implode( '\\n', $arr_msg );
                break;
            }

            fclose($fo);
        } while(0);
        return $ret;
    }

/********************************************************
* check_attributes
********************************************************/
    function check_attributes ()
    {
        $ret=0;
        $this->retmsg = '';
        do {
            $a_colshort=0; $a_name=1; $a_entry=2; $a_cat=3; $a_type=4; $a_descr=5;
            $arr_msg = array();
            $f2=fopen($this->attrs_file,"r");
            $data = fgetcsv($f2,0,"\t");
            while ( ($data = fgetcsv($f2,0,"\t") ) !== FALSE ) {
                 if (strlen($data[$a_cat])>0 && ! in_array($data[$a_cat], $this->categories)) {
                    array_push($arr_msg, "Unknown category '".$data[$a_cat]."' for attribute '".$data[$a_name]."'. See note (6.2)");
                    $ret=1;
                    continue;
                 }
                 if (! in_array($data[$a_type], $this->attrtypes)) {
                    array_push($arr_msg, "Unknown type '".$data[$a_type]."' for attribute '".$data[$a_name]."'. See note (6.3)");
                    $ret=1;
                    continue;
                 }
                 if ( strlen($data[$a_descr])>0 && ! preg_match("/^[a-zA-Z0-9_(),:\-\[\]{}+%!|?\.\*\/ µ°]+$/", $data[$a_descr]) ) {
                    array_push($arr_msg, "Descritpion for attribute '".$data[$a_name]."' should follow the convention described in note (6.4)");
                    $ret=1;
                    continue;
                 }
                 $arr2test =array('subset'=>$data[$a_colshort], 'attribute'=>$data[$a_name], 'entry'=>$data[$a_entry]);
                 foreach ($arr2test as $key => $val)
                    if ( strlen($val)>0 && ! preg_match("/^[a-zA-Z][a-zA-Z0-9_]+$/", $val)) {
                       array_push($arr_msg, "$key '$val' should follow the naming convention described in note (6.1)");
                       $ret=1;
                    }
            }
            fclose($f2);
            if ($ret) {
                $this->retmsg = implode( '\\n', $arr_msg );
                break;
            }
        } while(0);
        return $ret;
    }

/********************************************************
* check_subset
********************************************************/
    function check_subset ($subsetfile)
    {
        $ret=0;
        $this->retmsg = '';
        $tsvfile=$this->dir_input.'/'.$subsetfile;
        $arr_subset = array();
        do {
            $fo=fopen($tsvfile,"r");
            if ( ($data = fgetcsv($fo,0,"\t") ) == FALSE ) {
               $this->retmsg = "the file does not seem to be compliant with the TSV standard, See note (5)";
               $ret=1;
               break;
            }
            $arr_attr = array();
            for ($i = 0; $i < count($data); $i++ ) array_push($arr_attr, trim($data[$i]));
            fclose($fo);

            $s_colshort=2; $s_colid=3; $s_colfname=4;
            $f1=fopen($this->subsets_file,"r");
            while ( ($data = fgetcsv($f1,0,"\t") ) !== FALSE )
                  if ($data[$s_colfname] == $subsetfile) { $arr_subset = $data; break; }
            fclose($f1);

            if ( count($arr_subset) != count($this->def_infos['s_subsets']) ) {
               $this->retmsg = "the shortname '$subsetfile' seems not to be declared in the s_subsets.tsv file, See note (4.1)";
               $ret=1;
               break;
            }
            $a_colshort=0; $a_name=1; $a_entry=2; $a_cat=3; $a_type=4;
            $arr_msg = array();
            $f2=fopen($this->attrs_file,"r");
            $fattrsub=0;
            while ( ($data = fgetcsv($f2,0,"\t") ) !== FALSE ) {
                 if ($data[$a_colshort] !== $arr_subset[$s_colshort]) continue;
                 $fattrsub++;
                 if (! in_array(trim($data[$a_name]), $arr_attr)) {
                    array_push($arr_msg, "Unknown attribute '".$data[$a_name]."'");
                    $ret=1;
                    continue;
                 }
            }
            fclose($f2);
            if ($fattrsub==0) {
                $this->retmsg = "The '".$arr_subset[$s_colshort]."' subset seems not to be declared in the a_attributes.tsv file, See note (4.2)";
                $ret=1;
                break;
            }
            if ( count($arr_attr) < $fattrsub ) {
               $this->retmsg = "the column count in the '$subsetfile' file is inconsistent with that declared in the a_attributes.tsv file, See note (4.3)";
               $ret=1;
               break;
            }
            if ($ret) {
                $this->retmsg = implode( '\\n', $arr_msg );
                break;
            }
        } while(0);
        return $ret;
    }

/********************************************************
* build_hashDS: Building of the data structure (mainly as a hash table) 
* by parsing both metadata files, (s_subsets.tsv and a_attributes.tsv)
********************************************************/
    function build_hashDS ()
    {
        $ret=0;
        do {
            if ( $this->check_deffile('s_subsets') ) {
                $this->retmsg = 's_subsets: '.$this->retmsg;
                $this->ret=1;
                break;
            }
            if ( $this->check_deffile('a_attributes') ) {
                $this->retmsg = 'a_attributes: '.$this->retmsg;
                $this->ret=1;
                break;
            }
            # Parsing of the subset definition file
            $rank = array();
            $father_rank = array();
            $subsets = array();
            ini_set('auto_detect_line_endings',TRUE);
            $fo=fopen($this->subsets_file,"r");
            $cntfiles=0;
            while ( ($data = fgetcsv($fo,0,"\t") ) !== FALSE ) {
                if ($data[0] == "rank") continue;
                array_push($rank, $data[0]);
                array_push($father_rank, $data[1]);
                array_push($subsets, $data[2]);
                $idfile=0;
                if ($cntfiles==0 || ! array_key_exists( $data[4], array_flip($this->FILES) )) {
                    $cntfiles++;
                    $this->FILES[$cntfiles] = $data[4];
                    $idfile=$cntfiles;
                } else {
                    $arr_tmp = array_flip($this->FILES);
                    $idfile=$arr_tmp[ $data[4] ];
                }
                $this->hDataset[$data[2]] = array('idset' => $data[0], 'idfather' => $data[1],
                                            'name' => $data[5], 'entries' => array(), 'entities' => array(),
                                            'categories' => array(), 'file' => $idfile, 'filename' => $this->FILES[$idfile],
                                            'identifier' => $data[3],
                                            'cv_term_id' => $data[6], 'cv_term_name' => $data[7],
                                            'joinkey' => '' );
                foreach ($this->categories as $cat) $this->hDataset[$data[2]]['categories'][$cat] = array();
            }
            fclose($fo);

            # Building all the  possible sets of subsets (each subset within a set linked by a SQL JOIN )
            $this->hDataset['_dstree_'] = array();
            $i=count($subsets)-1;
            while ($i>=0) {
                $arr_tmp = array();
                $k=$i;
                while ($k>0) {
                    if (($father_rank[$k]-1)>=0)
                       array_push($arr_tmp, $subsets[$father_rank[$k]-1]);
                    $k = $father_rank[$father_rank[$k]-1];
                }
                $arr_tmp = array_reverse($arr_tmp);
                array_push($arr_tmp, $subsets[$i]);
                $this->hDataset['_dstree_'][$i+1] = $arr_tmp;
                if ($i>0) $this->hDataset[$subsets[$i]]['joinkey'] = $this->hDataset[$subsets[$father_rank[$i]-1]]['identifier'];
                $i--;
            }

            # Parsing of the attribute definition file
            $fo=fopen($this->attrs_file,"r");
            while ( ($data = fgetcsv($fo,0,"\t") ) !== FALSE ) {
                if ($data[0] == "subset") continue;
                $subset=$data[0];
                if( strlen($data[2])>0 ) $this->hDataset[$subset]['entries'][$data[2]] = $data[1];
                $this->hDataset[$subset]['entities'][$data[1]] = array( $data[5], $data[4], $data[6], $data[7] );
                if ( array_key_exists($data[3], $this->hDataset[$subset]['categories']) )
                     array_push($this->hDataset[$subset]['categories'][$data[3]], $data[1]);
            }
            fclose($fo);
            ini_set('auto_detect_line_endings',FALSE);

        } while(0);

        return $ret;
    }

/********************************************************
* getSQLparts
********************************************************/
    function getSQLparts ($ds,$func,$value,$op='')
    {
       $falias = 'f'.$this->hDataset[$ds]['file'];
       $file = $this->FILES[ $this->hDataset[$ds]['file'] ].' '.$falias;
       $entities = $this->hDataset[$ds]['entities'];
       $where ='';
       if (strlen($func)>0 && array_key_exists($func, $this->hDataset[$ds]['entries'])) {
          $key = $this->hDataset[$ds]['entries'][$func];
          $field = $falias.'.'.$key;
          $type = $entities[$key][1];
          if (strlen("$value")>0) {
             if ($type=='string') $value = "'$value'";
             $where .= "$field$op=$value";
          }
       }
       $fields = '';
       $i=0;
       foreach ($entities as $key => $arr) {
           if ($i>0) $fields .= ', ';
           $fields .= "$falias.$key";
           if ($this->FIELD_LIMIT>0 && $i>$this->FIELD_LIMIT) break;
           $i++;
       }
       return array($falias, $file, $fields, $where);
    }

/********************************************************
* getSelect1
********************************************************/
    function getSelect1($ds,$func,$value,$op='')
    {
       list ($falias, $file, $fields, $where) = $this->getSQLparts ($ds,$func,$value,$op);
       $query = "select $fields from $file";
       if (strlen("$where")>0) $query .= " where $where";
       return $query;
    }

/********************************************************
* getSelectMulti
********************************************************/
    function getSelectMulti($tarr_DS, $func,$value,$op='')
    {
       $gfields='';  $gfrom=''; $gwhere=''; $query='';
       do {

          $nbds = count($tarr_DS);
          if ($nbds==0) break;

          $dstree = array();
          if (count($tarr_DS)==1) {
             foreach ($this->hDataset['_dstree_'] as $keyid => $arr ) {
                if ($arr[count($arr)-1] == $tarr_DS[0]) {
                   $dstree = $arr;
                   break;
                }
             }
          } else {
             for ($i = 0; $i <= $nbds-1; $i++ ) {
                 $dsarr = array();
                 foreach ($this->hDataset['_dstree_'] as $keyid => $arr ) {
                      if ($arr[count($arr)-1] == $tarr_DS[$i]) {
                         $dsarr = $arr;
                         break;
                      }
                 }
                 if (count($dsarr)==0) continue;
                 foreach ($dsarr as $ds)
                     if ( ! in_array ( $ds, $dstree) ) array_push($dstree, $ds);
             }
          }
          if (count($dstree)==0) break;
          #$query='OKquery: '.implode('/',$tarr_DS).', dstree: '.implode('/',$dstree); break;

          # find the dataset to which the 'func' field belongs to
          $tAlias = array();
          $nbds=count($dstree);
          for ($i = 0; $i <= $nbds-1; $i++ ) {
               $ds=$dstree[$i];
               list ($falias, $file, $fields, $where) = $this->getSQLparts ($ds,$func,$value,$op);
               if (strlen($gfields)>0) $gfields .= ', ';
               $gfields .= $fields;
               array_push($tAlias,$falias);
               if (strlen($gfrom)>0) {
                    $joinkey = $this->hDataset[$ds]['joinkey'];
                    $k=$i-1;
                    if ($this->hDataset[$dstree[$k]]['joinkey'] != $joinkey) {
                       $k=0;
                       while($k<($i-1)) {
                          if ($this->hDataset[$dstree[$k]]['identifier'] == $joinkey) break;
                          $k++;
                       }
                    }
                    $joincond = "(".$tAlias[$k].".$joinkey=$falias.$joinkey)";
                    $tmp = str_replace("$falias.$joinkey, ","", $gfields);
                    $gfields = $tmp;
                    $gfrom .= " JOIN $file ON $joincond";
               } else {
                    $gfrom = "$file";
               }
               if (strlen($where)>0) {
                    $gwhere = $where;
               }
          }
          $query = "select $gfields from $gfrom";
          if (strlen($gwhere)>0) { $query .= " where $gwhere"; }

       } while(0);
       return $query;
    }

/********************************************************
* getCategoryList
********************************************************/
    function getCategoryList($tarr_DS, $category, $multi=1)
    {
       $list = array();
       $dstree = array();
       do {
          $nbds = count($tarr_DS);
          if ($nbds==0) break;

          if ( in_array($category, $this->categories) ) {
              if ($nbds==1 && $multi==1) {
                 foreach ($this->hDataset['_dstree_'] as $keyid => $arr ) {
                    if ($arr[count($arr)-1] == $tarr_DS[0]) {
                       $dstree = $arr;
                       break;
                    }
                 }
              }
              elseif ($nbds==1 && $multi==0) {
                 $dstree = $tarr_DS;
              }
              else {
                 for ($i = 0; $i <= $nbds-1; $i++ ) {
                     $dsarr = array();
                     foreach ($this->hDataset['_dstree_'] as $keyid => $arr ) {
                          if ($arr[count($arr)-1] == $tarr_DS[$i]) {
                             $dsarr = $arr;
                             break;
                          }
                     }
                     if (count($dsarr)==0) continue;
                     foreach ($dsarr as $ds)
                         if ( ! in_array ( $ds, $dstree) ) array_push($dstree, $ds);
                 }
              }
              if (count($dstree)==0) break;

              $cnt=0;
              foreach ($dstree as $ds) {
                 if (! isset($this->hDataset[$ds]['categories'][$category]) ) continue;
                 $arr_cat = $this->hDataset[$ds]['categories'][$category];
                 if (count($arr_cat)==0) continue;
                 foreach ($this->hDataset[$ds]['categories'][$category] as $field) {
                       $list[$cnt++] = array_merge( array($ds,$field), $this->hDataset[$ds]['entities'][$field] );
                 }
              }
          }
       } while(0);
       return $list;
    }

/********************************************************
* getEntryList
********************************************************/
    function getEntryList($tarr_DS, $multi=1)
    {
       $list = array();
       if (count($tarr_DS)==1 && $multi==1) {
          foreach ($this->hDataset['_dstree_'] as $keyid => $arr ) {
             if ($arr[count($arr)-1] == $tarr_DS[0]) {
                $tarr_DS = $arr;
                break;
             }
          }
       }
       $cnt=0;
       foreach ($tarr_DS as $keyid => $ds) {
          if (! isset($this->hDataset[$ds]['entries']) || count($this->hDataset[$ds]['entries'])==0 ) continue;
          $hEntry = array();
          foreach ($this->hDataset[$ds]['entries'] as $entry => $attributes) {
               if ( ! array_key_exists($entry, $hEntry) ) { $hEntry[$entry]=$attributes; $list[$cnt++] = array( $ds, $attributes, $entry ); }
          }
       }
       return $list;
    }

/********************************************************
* getSubsetList
********************************************************/
    function getSubsetList()
    {
       $list = array();
       $cnt=0;
       foreach ($this->hDataset as $subset => $arr ) {
           if (is_array($arr) && isset($arr['name'])) {
               $identifier = $arr['categories']['identifier'][0];
               $ent_rev = array_flip($arr['entries']);
               $list[$cnt++] = array ( $subset, $arr['name'], $identifier, $ent_rev[$identifier],
                                       $arr['idset'], $arr['idfather'], $arr['cv_term_id'], $arr['cv_term_name'] );
           }
       }
       return $list;
    }

/********************************************************
* getdata_xml_output: Generate the results within a XML structure
********************************************************/
    function getdata_xml_output($tsvfile, $xsl_url_file)
    {
       $nbrows=0;
       $ds_rows="";
       $fo=fopen($tsvfile,"r");
       if ( ($headers = fgetcsv($fo,0,"\t") ) !== FALSE) {
           $nbcol = count($headers);
           while ( ($data = fgetcsv($fo,0,"\t") ) !== FALSE ) {
               $nbrows++;
               $ds_rows .= "<Row>";
               for ($i = 0; $i < $nbcol; $i++ ) {
                  $elem_xml=$headers[$i];
                  $elem_value=$data[$i];
                  $elem_value = preg_replace('/&/', '&amp;', $elem_value);
                  $ds_rows .= "<$elem_xml>$elem_value</$elem_xml>";
               }
               $ds_rows .= "</Row>";
           }
           if (strlen($ds_rows)==0) $ds_rows = "<Row><Result>No results</Result></Row>";
       }
       fclose($fo);

       $xml_output  = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n<?xml-stylesheet type=\"text/xsl\" href=\"$xsl_url_file\"?>\n";
       $xml_output .= "<Array><Rows count=\"$nbrows\">$ds_rows</Rows></Array>\n";
       return $xml_output;
    }

/********************************************************
* getdata_json_output: Generate the results within a json structure
********************************************************/
    function getdata_json_output($tsvfile)
    {
       $nbrows=0;
       $hlist = array();
       $fo=fopen($tsvfile,"r");
       if ( ($headers = fgetcsv($fo,0,"\t") ) !== FALSE) {
           $nbcol = count($headers);
           while ( ($data = fgetcsv($fo,0,"\t") ) !== FALSE ) {
               $arr = array();
               for ($i = 0; $i < $nbcol; $i++ )
                  $arr[$headers[$i]]=$data[$i];
             $hlist[$nbrows++] = $arr;
           }
       }
       fclose($fo);
       return json_encode($hlist);
    }

/********************************************************
* cmd_Batch: Generate the command batch file under Windows
********************************************************/
    function cmd_Batch($filebatch, $CMD, $stdout, $stderr)
    {
       $content = "@echo off
$CMD  > $stdout 2> $stderr
setlocal
set file=$stderr
set maxbytesize=10

FOR /F \"usebackq\" %%A IN ('%file%') DO set size=%%~zA
FOR /F \"usebackq\" %%B IN (`dirname %file%`) DO set TEMP=%%B
cd %TEMP%

if %size% LSS %maxbytesize% (
    echo.0
    exit /b 0
) ELSE (
    echo.1
    exit /b 1
)
";
       file_put_contents($filebatch, $content);
    }

/********************************************************
* genKEY: Generate a KEY (30  alpha-num. char. )
********************************************************/
    function genKEY() {
       return md5(uniqid(rand(),true));
    }

/********************************************************
* build_sqlite_db: build the sqlite3 db into the data folder
********************************************************/
    function build_sqlite_db($dbindex,$debug=0)
    {
     # Init
       $stderr = $this->TMPDIR.'/'.$this->SESSID.'.err';
       $stdout = $this->TMPDIR.'/'.$this->SESSID.'.out';
       $dbcmd =  $this->TMPDIR.'/'.$this->SESSID.'.cmd';
       $db = $this->dir_input.'/'.$this->DBNAME;
       $extfile = $this->EXTFILES;
       $SQLOPT = $this->SQLOPT;

     # SQL statements for creating tables
       $scan = scandir($this->dir_input);
       foreach($scan as $file) {
           if (is_dir($this->dir_input.'/'.$file) || ! preg_match("/\.$extfile/", $file)>0) continue;
           $line = ".import $file ".basename($file,".$extfile");
           `echo $line >> $dbcmd`;
       }
     # SQL statements for creating indexes
       if ($dbindex) foreach ($this->hDataset as $subset => $arr ) {
           if ($subset=='_dstree_') continue;
           $line = "CREATE INDEX $subset"."_idx ON ".basename($arr['filename'],".$extfile")." (".$arr['identifier'].");";
           if ($this->OS == "LINUX")
               `echo "$line" >> $dbcmd`;
           else
               `echo $line >> $dbcmd`;
       }

       if ($debug==0) {
     # Execute the SQL statements if not debug
           if (is_file($db)) @unlink($db);
           $retval = is_file($db) ? 1 : 0 ;
           if (!$retval) {
              if ($this->OS == "LINUX") {
                 $CMD="( cd ".$this->dir_input."; cat $dbcmd | ".$this->SQLITE." $SQLOPT ".$this->DBNAME." )";
                 $retval=trim(`$CMD 1>$stdout 2>$stderr; echo \$?`);
              } else {
                 $sqlite = preg_replace('|/|', '\\', $this->SQLITE);
                 $CMD = "cd ".$this->dir_input." && cat $dbcmd | $sqlite $SQLOPT ".$this->DBNAME;
                 $filebatch = dirname($stdout).'/'.$this->SESSID.'.bat';
                 $this->cmd_Batch($filebatch, $CMD, $stdout, $stderr);
                 $retval=trim(`$filebatch`);
              }
           } else
              `echo -n "Permission denied" > $stderr` ;
       } else {
     # Return the full SQL statements if debug
           $retval=trim(`cat $dbcmd`);
       }
       return $retval;
    }

/********************************************************
* do_SQL_query: Execute the SQL query on the dataset $dsname
********************************************************/
    function do_SQL_query($query, $qopt='', $debug=0)
    {
     # Init
       $stderr = $this->TMPDIR.'/'.$this->SESSID.'.err';
       $stdout = $this->TMPDIR.'/'.$this->SESSID.'.out';
       $dbcmd =  $this->TMPDIR.'/'.$this->SESSID.'.cmd';
       $QSQL = $this->QSQL;
       $SQLOPT = $this->SQLOPT;
       $extfile = $this->EXTFILES;
       $db = $this->dir_input.'/'.$this->DBNAME;
 
     # Execute the SQL query if not debug
       do {
        # Linux / MaxOS system
          if ($this->OS == "LINUX") {
              if (is_file($db) && strlen($qopt)==0) {
              # Mode sqlite : apply the query directly on the db
                  $query = preg_replace("/\.$extfile/", '', $query);
                  $CMD ="echo \".headers on\n$query;\" | ".$this->SQLITE." $SQLOPT $db | uniq";
                  if ($debug>0) break;
              } else {
              # Mode no sqlite : apply the query help with q tool
                  $CMD = "(cd ".$this->dir_input."; $QSQL $qopt \"$query\" | uniq)";
                  if ($debug>0) break;
              }
              $retval=trim(`$CMD 1>$stdout 2>$stderr; ls -l $stderr | cut -d' ' -f5`);
              if ($retval) {
                  $CMD = "(cat $stderr |  sed -e \"s|.*Error|Error|\")";
                  $this->error_msg = trim(`$CMD`);
              }
              break;
          }

        # Windows 10 system
          if ($this->OS == "WIN") {
              if (is_file($db) && strlen($qopt)==0) {
              # Mode sqlite : apply the query directly on the db
                  $sqlite = preg_replace('|/|', '\\', $this->SQLITE);
                  $query = preg_replace("/\.$extfile/", '', $query);
                  `echo .headers on > $dbcmd`; 
                  if ($this->TRANSAC) `echo BEGIN TRANSACTION; >> $dbcmd`;
                  `echo $query ; >> $dbcmd`;
                  if ($this->TRANSAC) `echo COMMIT; >> $dbcmd`;
                  $CMD = "cd ".$this->dir_input." && cat $dbcmd | $sqlite $SQLOPT ".$this->DBNAME;
                  if ($debug>0) break;
              } else {
              # Mode no sqlite : apply the query help with q tool
                  $CMD = "cd ".$this->dir_input." && $QSQL $qopt \"$query\"";
                  if ($debug>0) break;
              }
              $filebatch = dirname($stdout).'/'.$this->SESSID.'.bat';
              $this->cmd_Batch($filebatch, $CMD, $stdout, $stderr);
              $retval=trim(`$filebatch`);
              if ($retval==0) break;
              $CMD = "type $stderr";
              $this->error_msg = trim(`$CMD`);
          }
       } while(0);

     # Return the full command if debug
       if ($debug>0) $retval=$CMD;

       return $retval;
    }
    
/********************************************************
* echo_results: Format the result and send it to the output
*     the result could be a specified file or the stdout file
********************************************************/
    function echo_results($format, $file='') {
          $stdout = $this->TMPDIR.'/'.$this->SESSID.'.out';
          if (strlen($file)==0) $file=$stdout;
          if ($format == 'xml') {
              header('Content-Type: text/xml; charset=ISO-8859-1');
              echo $this->getdata_xml_output($file, $this->GETDATA_URL_PROXY."xsl/query_report2.xsl");
          }
          elseif ($format == 'json') {
              header('Content-Type: application/json; charset=ISO-8859-1');
              echo $this->getdata_json_output($file);
          }
          elseif ($format == 'tsv' || $format == 'txt') {
              header('Content-Type: text/tab-separated-values; charset=ISO-8859-1');
              header('Content-Disposition: inline; filename="data-'.$this->genKEY().'.txt"');
              echo `cat $file`;
          }
          elseif ($format == 'csv') {
              header('Content-Type: text/csv; charset=ISO-8859-1');
              header('Content-Disposition: inline; filename="data-'.$this->genKEY().'.csv"');
              echo `cat $file | sed -e "s/\t/,/g"`;
          }
          elseif ($format == 'ssv') {
              header('Content-Type: text/csv; charset=ISO-8859-1');
              header('Content-Disposition: inline; filename="data-'.$this->genKEY().'.csv"');
              echo `cat $file | sed -e "s/\t/;/g"`;
          }
    }


/********************************************************
* getJSONDataTable : Create a simple container format for describing a coherent
     collection of data in a single 'package'.  It provides the basis for convenient
     delivery, installation and management of datasets.
     cf https://specs.frictionlessdata.io/data-package/
********************************************************/
    function _get_lists_of_threads($rank)
    {
        $threads = array();
        foreach ($this->hDataset as $subset => $arr ) {
            if ($subset=='_dstree_') continue;
            if ( $arr['idfather'] != $rank ) continue;
            array_push($threads, $subset );
        }
        return $threads;
    }
    function getJSONDataTable($urlroot='', $format='tsv')
    {
        $subsets = array_keys($this->hDataset);
    # name / profile / datapackage_version / keywords / licenses
        $json  ='{"name":"'.$this->dsname.'",';
        $json .='"profile":"'.$this->DPKG_INFOS['URL_JSONDEF'].'odam-data-package.json",';
        $json .='"datapackage_version":"'.$this->DPKG_INFOS['DATAPACKAGE_VERSION'].'", "keywords":["Experimental Data Tables", "ODAM"],';
        $license = explode(',', $this->DPKG_INFOS['LICENSE']);
        $json .='"licenses":[{"name":"'.trim($license[0]).'", "path":"'.trim($license[1]).'", "title":"'.trim($license[2]).'"}],';
    # resources
        $json .='"resources":[';
        $flg=0;
        foreach ($this->hDataset as $subset => $arr ) {
            if ($subset=='_dstree_') continue;
            if (is_array($arr) && isset($arr['name'])) {
                if ($flg) $json .=',';
                $flg++;
                $identifier = $arr['categories']['identifier'][0];
                $ent_rev = array_flip($arr['entries']);
          # path / profile / name / title / identifier / obtainedFrom / joinkey / CV Term
                if (strlen($urlroot)>0)
                    $json .='{"path":"'."$urlroot/$format/".$this->dsname.'/'.$subset.'", ';
                else
                    $json .='{"path":"'.$arr['filename'].'", ';
                $json .='"profile": "'.$this->DPKG_INFOS['URL_JSONDEF'].'odam-data-resource.json", ';
                $json .='"name":"'.$subset.'", ';
                $json .='"title":"'.$arr['name'].'", ';
                $json .='"identifier":"'.$identifier.'", ';
                if ($arr['idfather']>0)
                    $json .='"obtainedFrom":"'.$subsets[$arr['idfather']-1].'", ';
                if (strlen($arr['joinkey'])>0)
                    $json .='"joinkey":"'.$arr['joinkey'].'", ';
                $json .='"cv_term": { "label":"'.$arr['cv_term_name'].'", "path":"'.$arr['cv_term_id'].'" }, ';
          # schema - fields
                $json .='"schema": {"fields": [';
                $i=0;
                foreach ($arr['entities'] as $field => $subarr ) {
                    if ($i) $json .=','; $i++;
                    $typefield = $subarr[1] == "string" ? "string" : "number" ;
                    $json .='{"name":"'.$field.'", "type":"'.$typefield.'"';
                    if (preg_match('/\((.+)\)/', $subarr[0], $matches, PREG_OFFSET_CAPTURE))
                        $json .=', "title":"'.trim(preg_replace('/\(.+\)/', '', $subarr[0])).'", "unit":"'.$matches[1][0].'"';
                    else
                        $json .=', "title":"'.trim($subarr[0]).'"';
                    if (strlen($subarr[2]))
                        $json .=', "cv_term": { "label":"'.$subarr[3].'", "path":"'.$subarr[2].'" }';
                    $required = $field==$identifier || $field==$arr['joinkey'] ? 'true' : 'false' ;
                    $json .= ', "constraints": { "required":'.$required.' }';
                    $json .='}';
                }
                $json .='],';
          # schema - primaryKey
                $json .='"primaryKey": [ "'.$identifier.'" ], ';
          # schema - foreignKey
                $threads = $this->_get_lists_of_threads($arr['idset']);
                if (count($threads)>0) {
                    $json .='"foreignKeys": [ ';
                    $i=0;
                    foreach($threads as $thread) {
                        if ($i) $json .=','; $i++;
                        $json .= '{ "fields": "'.$identifier.'", "reference": {"resource": "'.$thread.'", "fields": "'.$identifier.'"} }';
                    }
                    $json .='],';
                }
          # schema - categories
                $i=0;
                $json .='"categories": [';
                foreach ($arr['categories'] as $cat => $subarr ) {
                    if (count($subarr)==0) continue;
                    if ($i) $json .=','; $i++;
                    $json .='{"name":"'.$cat.'",';
                    $json .='"fields":["'.join('","',$subarr).'"]}';
                }
                $json .=']},';
          # dialect
                $json .='"dialect": {"caseSensitiveHeader": true, "delimiter":"\t", "decimalChar":".", "lineTerminator":"\n", "header":true, "doubleQuote": false, "skipInitialSpace": true, "commentChar":"#" },';
          # encoding / mediatype / missingValues
                $json .='"encoding":"utf-8", "format":"csv", "mediatype":"text/tab-separated-values", "missingValues":[""]}';
            }
        }
        $json .= ']';
    # sources
        if (strlen($urlroot)>0) {
            $json .=', "sources":[{"title":"ODAM Dataexplorer", "path":"'.dirname($urlroot).'/dataexplorer/?ds='.$this->dsname.'"}]';
        }
        $json .= '}';
        return $json;
    }

}

?>
