<?php
/* ID Set_Config.inc.php v1.0
 * Copyright (C) 2014 INRA - DJ
 *
 */

class Set_Config {

   /* 
    * Fichier de configuration global.
    * 2 types de section : ENV et GLOBAL
    * Section ENV : declaration des variables d'environnement
    *     (ajoutees au tableau global $_ENV)
    * 
    * Section GLOBAL : declaration des variables globales
    *     (accessible comme attribut de l'instance de l'objet $objet->VAR)
    * A cela s'ajoute un sous-type de section, les '_OTHERS' :
    *  ENV_OTHERS et GLOBAL_OTHERS identiques dans leur interpretation
    *  repectivement comme ENV et GLOBAL. Par défaut, ces sections ne sont pas
    * prises en compte, sauf en positionnant le flag -other a 1. 
    */

    var $glob_conf = "/etc/conf/global.conf";

   /********************************************************
    * Methode d'initialisation de l'instance
    ********************************************************/
    function __construct($input) { $this->Set_Config($input); } # PHP 5.3-compliant

    function Set_Config($input=array('-others'=>FALSE,'-global'=>TRUE, '-session'=>FALSE)) {

        $this->others  = isset($input['-others'])  ? $input['-others']  : FALSE; 
        $this->global  = isset($input['-global'])  ? $input['-global']  : TRUE; 
        $this->session = isset($input['-session']) ? $input['-session'] : FALSE; 

        if ( $this->global && is_readable($this->glob_conf))
           $this->_read_configuration($this->glob_conf);
        if (isset($input['-conf']))
            $this->_read_configuration($input['-conf']);
    }

    // Positionnement d'une variable d'environnement
    function _Set_Env($var,$valeur) {
        $_ENV[$var]=$valeur;
        putenv("$var=$valeur");
    }

    // Positionnement d'une variable globale
    function _Set_Global($var,$valeur) {
        if ($this->session) {
           $_SESSION[$var]=$valeur; }
        else { 
           $this->$var=$valeur; }
    }

    /******************************************************** 
     * Parse le fichier de configuration (parametre d'entree)
     ********************************************************/
    function _read_configuration($file) {

    // Ouverture fichier
        $fd = fopen($file,"r") 
             or exit("Impossible d'ouvrir le fichier de configuration $file");
    // Pour chaque ligne
        while (!feof($fd)) {
           $line = fgets($fd,1024);

    // Traite le cas des lignes superflues 
           $line = preg_replace("/#.*/","",$line);
           $line = preg_replace("/\s+$/","",$line);
           $line = preg_replace("/^\s+/","",$line);
           if (! strlen($line) ) continue;

    // Traite le cas des entetes de section 
           if ( preg_match("/^\[(.*)\]$/",$line,$entete) ) {
                if ( ! preg_match("/^(\S+)_(\S+)$/",$entete[1],$asection) )
                   preg_match("/^(.*)$/",$entete[1],$asection);
                $section = $asection[1];
                $others  = isset($asection[2]) ? TRUE : FALSE; 
                continue;
           }

    // Traite le cas ou les sections 'OTHERS' doivent etre exclues (par defaut)
           if ( ! $this->others && $others ) continue;

    // Traite le cas des lignes d'affection selon la section
           if ( preg_match("/^(\S+)\s*=\s*(.*)$/",$line,$defvar) ) {
              $var    = $defvar[1];
              $valeur = $defvar[2];
              switch ($section):
                // Section ENV 
                case 'ENV':
                    $this->_Set_Env($var,$valeur);
                    break;
                // Section GLOBAL
                case 'GLOBAL':
                    $this->_Set_Global($var,$valeur);
                    break;
                // Section inconnue
                    $this->_Set_Global($var,$valeur);
              endswitch;

           }

        }
        fclose($fd);
    }

} // class

/******************************************************** 
 * Positionnement des variables gloables dans le tableau
 * global $_SESSION 
 ********************************************************/
function Set_Config_Session($input=array('-others'=>FALSE,'-global'=>TRUE,'-session'=>TRUE)) {
    new Set_Config($input);
}


?>
