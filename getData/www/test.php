#!/usr/bin/php
<?php
$DATADIR='/opt/data/FLAU_2021_02';
$extfile='txt';

$checksumfile = $DATADIR.'/checksum';
if (file_exists($checksumfile)) unlink($checksumfile);

$scan=scandir($DATADIR);
foreach($scan as $file) {
  if (is_dir($DATADIR.'/'.$file) || ! preg_match("/\.$extfile/", $file)>0) continue;
  $md5 = md5_file($DATADIR.'/'.$file);
  `echo $md5 >> $checksumfile`;
  if ( in_array( strtolower(substr($file, 0, strrpos($file, "."))), array('group', 'animals')) )
     echo substr($file, 0, strrpos($file, "."))."\n";

}

if (file_exists($checksumfile)) {
  $lines = explode("\n",file_get_contents($checksumfile));
  echo count($lines)."\n";

  $scan=scandir($DATADIR);
  $cnt=0;
  foreach($scan as $file) {
     if (is_dir($DATADIR.'/'.$file) || ! preg_match("/\.$extfile/", $file)>0) continue;
     $md5 = md5_file($DATADIR.'/'.$file);
     if ($md5 == $lines[$cnt]) echo "Success: checksum for $file is good.\n";
     $cnt++;
  }
}


echo "OK\n";

?>
