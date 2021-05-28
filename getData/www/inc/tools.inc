<?php
# Auteur(s): D.JACOB 
# Copyright: (c) 2015-2021 INRAE

$theIP       = $_SERVER["REMOTE_ADDR"];
$theURI      = $_SERVER["REQUEST_URI"];

$tFormat   = array('tsv', 'xml', 'json');

$stderr = "$TMPDIR/$SESSID.err";
$stdout = "$TMPDIR/$SESSID.out";

$sep = "\t";

# Generate a KEY (30  alpha-num. char. )
function genKEY() {
   return md5(uniqid(rand(),true));
}

function doQuery($dsname, $query, $debug=0) {
      global $OS, $DATADIR, $QSQL, $stdout, $stderr, $def;
      if ($OS == "LINUX") {
          $CMD = "(cd $DATADIR/$dsname; $QSQL \"$query\" | uniq)";
          if ($debug== 0) $retval=trim(`$CMD 1>$stdout 2>$stderr; ls -l $stderr | cut -d' ' -f5`);
      }
      if ($OS == "WIN") {
          $CMD = "cd $DATADIR/$dsname && $QSQL \"$query\"";
          $filebatch = dirname($stdout)."/cmdbatch.bat";
          $def->cmd_Batch($filebatch, $CMD, $stdout, $stderr);
          if ($debug== 0) $retval=trim(`$filebatch`);
      }
      if ($debug>0) $retval=$CMD;
      return $retval;
}

function echo_results($format, $file='') {
      global $stdout, $GETDATA_URL_PROXY, $def;
      if (strlen($file)==0) $file=$stdout;
      if ($format == 'xml') {
          header('Content-Type: text/xml; charset=ISO-8859-1');
          echo $def->getdata_xml_output($file, "$GETDATA_URL_PROXY"."xsl/query_report2.xsl");
      }
      elseif ($format == 'json') {
          header('Content-Type: application/json; charset=ISO-8859-1');
          echo $def->getdata_json_output($file);
      }
      elseif ($format == 'tsv') {
          header('Content-Type: text/tab-separated-values; charset=ISO-8859-1');
          header('Content-Disposition: inline; filename="data-'.genKEY().'.txt"');
          echo `cat $file`;
      }
}

?>