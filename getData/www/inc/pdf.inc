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
        echo "<pre>Error: No dataset $dataset</pre>";
        break;
    }

    $cntopt--;
    $pdf_name = array_shift($urloptions);
    if ($cntopt<1 || strlen($pdf_name)==0 ) {
        echo "<pre>Error : PDF name required</pre>";
        break;
    }

    $filename = "$DATADIR/$dataset/pdf/$pdf_name";
    if (! is_file($filename)) {
        echo "<pre>Error : PDF file $pdf_name.pdf does not exist</pre>";
        break;
    }

    // send the right headers
    header("Content-Type: application/pdf");
    header("Content-Length: " . filesize($filename));

    // Send the file to the browser.
    readfile($filename);

} while (0);



?>

