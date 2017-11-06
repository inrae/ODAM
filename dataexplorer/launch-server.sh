#!/bin/bash

(
  cd /srv/shiny-server

  RET=$(env | grep 'GETDATA_URL_PROXY' 1>/dev/null 2>/dev/null; echo $?)
  if [ $RET -eq 0 ]; then
      VAR=$( env | grep 'GETDATA_URL_PROXY' | cut -d'=' -f2 | tr -d "\n")
      sed -e "s|<<<GETDATA_EXTERNAL_URL>>|$VAR|" ./global.tmpl > ./global.R
  fi
)


prog=shiny-server
execute=$(which $prog)
logfile=/var/log/${prog}.log
lockfile=/var/run/${prog}.pid

echo "Starting shiny-server:" 
$prog --daemon "--pidfile=$lockfile" >> $logfile

