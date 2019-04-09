<?php
# Auteur(s): D.JACOB 
# Copyright: (c) INRA - 2015

$logfile     = "$ROOTDIR/log/rest.log";
$theIP       = $_SERVER["REMOTE_ADDR"];
$theURI      = $_SERVER["REQUEST_URI"];

$restmsg="no options";

$tFormat   = array('tsv', 'xml', 'json');

$stderr = "$TMPDIR/$SESSID.err";
$stdout = "$TMPDIR/$SESSID.out";

$sep = "\t";

function doQuery($dsname, $query, $debug=0) {
      global $DATADIR, $QBIN, $QOPTIONS, $stdout, $stderr;
      #$CMD = "(cd $DATADIR/$dsname; $QBIN $QOPTIONS \"$query\" | uniq | sed -e \"s/Â//g\")";
      $CMD = "(cd $DATADIR/$dsname; $QBIN $QOPTIONS \"$query\" | uniq)";
      if ($debug>0) {
         $retval=$CMD;
      } else {
         $retval=trim(`$CMD 1>$stdout 2>$stderr; ls -l $stderr | cut -d' ' -f5`);
      }
      return $retval;
}

function echo_results($format) {
      global $stdout, $GETDATA_URL_PROXY, $def;
      if ($format == 'xml') {
          header('Content-Type: text/xml; charset=ISO-8859-1');
          echo $def->getdata_xml_output($stdout, "$GETDATA_URL_PROXY"."xsl/query_report2.xsl");
      }
      elseif ($format == 'json') {
          header('Content-Type: text/json; charset=ISO-8859-1');
          echo `cat $stdout | /usr/bin/csvtojson --delimiter=';' `;
      }
      elseif ($format == 'tsv') {
          echo `cat $stdout`;
      }
}

?>