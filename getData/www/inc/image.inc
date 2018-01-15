<?php
# Auteur(s): D.JACOB 
# Copyright: (c) INRA - 2017

if ($_SERVER['PATH_INFO'])
do {

    $urloptions=array();
    $dataset='';
    $cntopt=0;

    # Parse PATH_INFO
    $urloptions = explode("/",$_SERVER['PATH_INFO']);
    array_shift($urloptions);
    $cntopt=count($urloptions);
    $dataset = array_shift($urloptions);

# Case of dataset not specify
    if (!strlen($dataset)) {
        break;
    }

    if (! is_dir("$DATADIR/$dataset")) {
        break;
    }

    $cntopt--;
    $image = array_shift($urloptions);
    if ($cntopt<1 || strlen($image)==0 ) {
        break;
    }

    $filename = "$DATADIR/$dataset/images/$image";
    if (! is_file($filename)) {
        break;
    }
    $fp = fopen($filename, 'rb');

    // send the right headers
    header("Content-Type: image/png");
    header("Content-Length: " . filesize($filename));

    // dump the picture and stop the script
    fpassthru($fp);

} while (0);



?>

