<?php
# Auteur(s): D.JACOB 
# Copyright: (c) INRA - 2017

do {

    $urloptions=array();
    $dataset='';
    $cntopt=0;

    $INFOS_TEXT = '';


    # Parse PATH_INFO if needed
    if ($_SERVER['PATH_INFO']) {
        $urloptions = explode("/",$_SERVER['PATH_INFO']);
        array_shift($urloptions);
        $cntopt=count($urloptions);
        $dataset = array_shift($urloptions);
    }

# Case of dataset not specify
    if (!strlen($dataset)) {
        $INFOS_TEXT = '## ERROR: no dataset provided';
        break;
    }

    if (! is_dir("$DATADIR/$dataset")) {
        $INFOS_TEXT = '## ERROR: dataset $dataset is not available';
        break;
    }

    if (! is_file("$DATADIR/$dataset/infos.md")) {
        $INFOS_TEXT = '## No dataset information';
        break;
    }

    $INFOS_TEXT = file_get_contents("$DATADIR/$dataset/infos.md");

} while (0);

header('Content-Type: text/markdown; charset=ISO-8859-1');
echo $INFOS_TEXT;


?>

