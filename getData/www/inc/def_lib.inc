<?php
/* ID def_lib.inc v1.0
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
     var $dsname       = "";
     var $FILES        = array ();
     var $hDataset     = array();
     var $subsets_file = "";
     var $attrs_file   = "";
     var $dir_input    = "";
     var $retmsg       = "OK";
     var $ret          = 0;

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
        $csvfile=$this->dir_input.'/'.$defname.'.tsv';
        $arr_col = $this->def_infos[$defname];
        $nbcol = count($arr_col);
        do {
            # files exist ?
            if (! is_file("$csvfile")) {
               $this->retmsg = "the file is missing";
               $ret=1;
               break;
            }
            $fo=fopen($csvfile,"r");
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
                 if ( strlen($data[$a_descr])>0 && ! preg_match("/^[a-zA-Z0-9_(),:\-\[\]{}+%!|?\.\*\/ µ]+$/", $data[$a_descr]) ) {
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
        $csvfile=$this->dir_input.'/'.$subsetfile;
        $arr_subset = array();
        do {
            $fo=fopen($csvfile,"r");
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
* build_hashDS: Building of the data structure (mainly as a hash table) by parsing both metadata files, (s_subsets.tsv and a_attributes.tsv)
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
    function getSQLparts ($ds,$func,$value)
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
             $where .= "$field=$value";
          }
       }
       $fields = '';
       $i=0;
       foreach ($entities as $key => $arr) {
           if ($i>0) $fields .= ', ';
           $fields .= "$falias.$key";
           $i++;
       }
       return array($falias, $file, $fields, $where);
    }

/********************************************************
* getSelect1
********************************************************/
    function getSelect1($ds,$func,$value)
    {
       list ($falias, $file, $fields, $where) = $this->getSQLparts ($ds,$func,$value);
       $query = "select $fields from $file";
       if (strlen("$where")>0) $query .= " where $where";
       return $query;
    }

/********************************************************
* getSelectMulti
********************************************************/
    function getSelectMulti($tarr_DS, $func,$value)
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
               list ($falias, $file, $fields, $where) = $this->getSQLparts ($ds,$func,$value);
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
    function getCategoryList($tarr_DS, $category)
    {
       $list = array();
       $dstree = array();
       do {
          $nbds = count($tarr_DS);
          if ($nbds==0) break;

          if ( in_array($category, $this->categories) ) {
              if ($nbds==1) {
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
    function getEntryList($tarr_DS)
    {
        $list = array();
        if (count($tarr_DS)==1) {
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
    function getdata_xml_output($csvfile, $xsl_url_file)
    {
        $nbrows=0;
        $ds_rows="";
        $fo=fopen($csvfile,"r");
        if ( ($headers = fgetcsv($fo,0,"\t") ) !== FALSE) {
            $nbcol = count($headers);
            while ( ($data = fgetcsv($fo,0,"\t") ) !== FALSE ) {
                $nbrow++;
                $ds_rows .= "<Row>";
                for ($i = 0; $i < $nbcol; $i++ ) {
                   $elem_xml=$headers[$i];
                   $elem_value=$data[$i];
                   $ds_rows .= "<$elem_xml>$elem_value</$elem_xml>";
                }
                $ds_rows .= "</Row>";
            }
        }
        fclose($fo);

        $xml_output  = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n<?xml-stylesheet type=\"text/xsl\" href=\"$xsl_url_file\"?>\n";
        $xml_output .= "<Array><Rows count=\"$nbrows\">$ds_rows</Rows></Array>\n";
        return $xml_output;
    }

/********************************************************
* getdata_json_output: Generate the results within a json structure
********************************************************/
    function getdata_json_output($csvfile)
    {
        $nbrows=0;
        $hlist = array();
        $fo=fopen($csvfile,"r");
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
* getJSONDataTable : Create a simple container format for describing a coherent 
     collection of data in a single 'package'.  It provides the basis for convenient 
     delivery, installation and management of datasets.
     cf https://specs.frictionlessdata.io/data-package/
********************************************************/
    function getJSONDataTable($urlroot='', $format='tsv')
    {
        $subsets = array_keys($this->hDataset);
        $json  ='{"name":"'.$this->dsname.'",';
        $json .='"profile":"odam-data-package",';
        $json .='"datapackage_version":"1.0", "keywords":["Experimental Data Tables"],';
        $json .='"licenses":[{"name":"ODC-BY-1.0", "path":"https://opendatacommons.org/licenses/by/", "title":"Open Data Commons Attribution License 1.0"}],';
        $json .='"resources":[';
        $flg=0;
        foreach ($this->hDataset as $subset => $arr ) {
            if ($subset=='_dstree_') continue;
            if (is_array($arr) && isset($arr['name'])) {
                if ($flg) $json .=',';
                $flg++;
                $identifier = $arr['categories']['identifier'][0];
                $ent_rev = array_flip($arr['entries']);
                if (strlen($urlroot)>0)
                    $json .='{"path":"'."$urlroot/$format/".$this->dsname.'/'.$subset.'", ';
                else
                    $json .='{"path":"'.$arr['filename'].'", ';
                $json .='"profile": "odam-data-resource", ';                
                $json .='"name":"'.$subset.'", ';
                $json .='"title":"'.$arr['name'].'", ';                           
                $json .='"identifier":"'.$identifier.'", ';
                if ($arr['idfather']>0)
                    $json .='"obtainedFrom":"'.$subsets[$arr['idfather']-1].'", ';
                if (strlen($arr['joinkey'])>0)
                    $json .='"joinkey":"'.$arr['joinkey'].'", ';
                $json .='"cv_term_id":"'.$arr['cv_term_id'].'", '; 
                $json .='"cv_term_name":"'.$arr['cv_term_name'].'", ';
                
                $json .='"schema": {"fields": [';
                $i=0;
                foreach ($arr['entities'] as $field => $subarr ) {
                    if ($i) $json .=','; $i++;
                    $json .='{"name":"'.$field.'", "type":"'.$subarr[1].'"';
                    if (preg_match('/\((.+)\)/', $subarr[0], $matches, PREG_OFFSET_CAPTURE))
                        $json .=', "title":"'.trim(preg_replace('/\(.+\)/', '', $subarr[0])).'", "unit":"'.$matches[1][0].'"';
                    else
                        $json .=', "title":"'.trim($subarr[0]).'"';
                    if (strlen($subarr[2]))
                        $json .=', "cv_term_id":"'.$subarr[2].'", "cv_term_name":"'.$subarr[3].'"';
                    $json .='}';
                }
                $json .='],';
                $i=0;
                $json .='"categories": [';
                foreach ($arr['categories'] as $cat => $subarr ) {
                    if (count($subarr)==0) continue;
                    if ($i) $json .=','; $i++;
                    $json .='{"name":"'.$cat.'",';
                    $json .='"fields":["'.join('","',$subarr).'"]}';
                }
                $json .=']},';
                $json .='"dialect": {"caseSensitiveHeader": true, "delimiter":"\t", "decimalChar":".", "lineTerminator":"\n", "header":true, "doubleQuote": false, "skipInitialSpace": true, "commentChar":"#" },';
                $json .='"encoding":"utf-8", "format":"tsv", "mediatype":"text/tab-separated-values", "missingValues":[""]}';
            }
        }
        $json .= ']}';
        return $json;
    }

}

/*

build_hashDS: Building of the data structure (mainly as a hash table) by parsing both metadata files, (s_subsets.tsv and a_attributes.tsv)

--------------------------------------------------------------------------------
 EXAMPLE OF AN OBJECT INSTANCE POPULATES WITH SOME SUBSETS OF THE FRIM1 DATASET
--------------------------------------------------------------------------------

$FILES      = array ( 1 => 'plants.csv', 2 => 'harvests.csv', 3 => 'samples.csv', 4 => 'compounds.csv' );

$hDataset = array(
     # List of differents possible pathways among the data subsets in order to optimize the building of the SQL queries.
     'dstree' => array(
                   1 => array('plants'), 
                   2 => array('plants', 'harvests' ),
                   3 => array('plants', 'harvests', 'samples' ),
                   4 => array('plants', 'harvests', 'samples', 'compounds' ),
                   5 => array('plants', 'harvests', 'samples', 'compoundsFW' )
     ),
     # data subset 'plants'
     'plants' => array (
                   'name'     => 'Plant identities',
                   'entries'  => array(
                                      'plantid' => 'PlanteID',
                                      'plant' => 'Plant',
                                      'row' => 'Row',
                                      'treatment' => 'Treatment'
                   ),
                   'entities' => array(
                                      'Genotype' => array('Genotype','string'),
                                      'PlanteID' => array('Plant Identifier','numeric'),
                                      'Plant' => array('Plant code name','string'),
                                      'Row' => array('Row of the plant on the table','string'),
                                      'Treatment' => array('Treatment', 'string'),
                   ),
                   'categories' => array(
                                      'identifier' => array('PlanteID'),
                                      'factor' => array('Treatment', 'Row'),
                                      'quantitative' => array(),
                                      'qualitative' => array('Row', 'Genotype')
                   ),
                   'file' => 1,
                   'joinkey' => ''
     ),
     # data subset 'harvests'
     'harvests' => array (
                   'name'     => 'Harvest features',
                   'entries'  => array(
                                      'lot' => 'Lot',
                                      'age' => 'FruitAge',
                                      'position' => 'FruitPosition',
                                      'truss' => 'Truss',
                                      'date' => 'HarvestDate'
                   ),
                   'entities' => array(
                                      'PlanteID' => array('Plant Identifier','numeric'),
                                      'Lot' => array('Lot identifier','numeric'),
                                      'Truss' => array('Rank identifier of truss','string'),
                                      'FruitAge' => array('Fruit Age(dpa targeted)','string'),
                                      'HarvestDate' => array('Harvest Date','string'),
                                      'FruitPosition' => array('Fruit Position','numeric'),
                                      'HarvestHour' => array('Harvest Hour','string'),
                                      'FruitFW' => array('Fruit Fresh Weight(g)','numeric'),
                                      'FruitDiameter' => array('Fruit Diameter (mm)','numeric'),
                                      'FruitHeight' => array('Fruit Height (mm)','numeric')
                   ),
                   'categories' => array(
                                      'identifier' => array('Lot'),
                                      'factor' => array('FruitAge', 'FruitPosition'),
                                      'quantitative' => array('FruitFW', 'FruitDiameter', 'FruitHeight'),
                                      'qualitative' => array('Truss', 'FruitPosition')
                   ),
                   'file' => 2,
                   'joinkey' => 'PlanteID'
      ),
     # data subset 'samples'
     'samples' => array (
                   'name'     => 'Sample features',
                   'entries'  => array(
                                      'sampleid' => 'SampleID'
                   ),
                   'entities' => array(
                                      'Lot' => array('Lot identifier','numeric'),
                                      'SampleID' => array('Sample identifier','numeric'),
                                      'NbFruit' => array('Fruit Number per sample','numeric'),
                                      'GellyFW' => array('Gelly Fred Weight(g) per sample','numeric'),
                                      'GellyFruit' => array('Gelly per Fruit (estimated g)','numeric'),
                                      'BER' => array('BER')
                   ),
                   'categories' => array(
                                      'identifier' => array('SampleID'),
                                      'factor' => array(),
                                      'quantitative' => array('GellyFW', 'GellyFruit'),
                                      'qualitative' => array()
                   ),
                   'file' => 3,
                   'joinkey' => 'Lot'
     ),
     # data subset 'compounds'
     'compounds' => array (
                   'name'     => 'quantifications of compounds (% weight/MIA)',
                   'entries'  => array(
                                      'sampleid' => 'SampleID',
                    ),
                   'entities' => array(
                                      'SampleID' => array('Sample identifier','numeric'),
                                      'MassBefore' => array('m av.extraction  (g)','numeric'),
                                      'MassMIA' => array('masse MIA (g)','numeric'),
                                      'RDT' => array('Rdt (% MIA/DW)','numeric'),
                                      'Starch1' => array('Dosage amidon (%poids/MIA)','numeric'),
                                      'Starch2' => array('amidon (g/gDW) ','numeric'),
                                      'RHAMNOSE' => array('RHAMNOSE','numeric'),
                                      'ARABINOSE' => array('ARABINOSE','numeric'),
                                      'XYLOSE' => array('XYLOSE','numeric'),
                                      'MANNOSE' => array('MANNOSE','numeric'),
                                      'GALACTOSE' => array('GALACTOSE','numeric'),
                                      'GLUCOSE' => array('GLUCOSE (- % amidon)','numeric'),
                                      'OsesN' => array('Oses N (% poids total)','numeric'),
                                      'PoidsAU' => array('%poids AU/MIA','numeric'),
                                      'PoidsTotal' => array('% poids total ','numeric'),
                                      'PP' => array('Polysaccharides des parois (%)','numeric')
                   ),
                   'categories' => array(
                                      'identifier' => array('SampleID'),
                                      'factor' => array(),
                                      'quantitative' => array('MassBefore', 'MassMIA', 'RDT', 'Starch1', 'Starch2', 'RHAMNOSE', 'ARABINOSE', 'XYLOSE', 'MANNOSE', 'GALACTOSE', 'GLUCOSE', 'OsesN', 'PoidsAU', 'PoidsTotal', 'PP')
                   ),
                   'file' => 4,
                   'joinkey' => 'SampleID'
     ),
     # data subset 'compoundsFW'
     'compoundsFW' => array (
                   'name'     => 'quantifications of compounds (FW unit)',
                   'entries'  => array(
                                      'sampleid' => 'SampleID',
                    ),
                   'entities' => array(
                                      'SampleID' => array('Sample identifier','numeric'),
                                      'RhamnoseFW' => array('Rhamnose FW','numeric'),
                                      'ArabinoseFW' => array('Arabinose FW','numeric'),
                                      'XyloseFW' => array('Xylose FW','numeric'),
                                      'MannoseFW' => array('Mannose FW','numeric'),
                                      'GalactoseFW' => array('Galactose FW','numeric'),
                                      'GlucoseFW' => array('Glucose FW','numeric'),
                                      'OsesAcidsFW' => array('oses acides FW','numeric')
                   ),
                   'categories' => array(
                                      'identifier' => array('SampleID'),
                                      'factor' => array(),
                                      'quantitative' => array('RhamnoseFW', 'ArabinoseFW', 'XyloseFW', 'MannoseFW', 'GalactoseFW', 'GlucoseFW', 'OsesAcidsFW')
                   ),
                   'file' => 4,
                   'joinkey' => 'SampleID'
     )
);

*/
?>

