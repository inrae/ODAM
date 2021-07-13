#!/bin/bash

INI=/srv/shiny-server/conf/global.ini
[ ! -f $INI ] && echo "ERROR: File $INI does not exist" && exit 1

for l in `env`; do 
    e=$(echo "$l" | sed -e 's/\//\\&/g; 1{$s/^$/""/}; 1!s/^/"/; $!s/$/"/')
    k=`echo $e | cut -d'=' -f1`
    grep -E "^$k=" $INI 1>/dev/null 2>/dev/null
    [ $? -eq 0 ] && sed -i -e "s/^$k=.*$/$e/" $INI
done

MAXSESSION=`cat $INI | grep MAXSESSION | cut -d'=' -f2 | tr -d "\n"`
echo -n "Limits the maximum number of sessions to $MAXSESSION..."
CONF=/etc/shiny-server/shiny-server.conf
grep -E '.*simple_scheduler [0-9]+;' $CONF 1>/dev/null 2>/dev/null
[ $? -eq 0 ] && sed -i -e "s/simple_scheduler [0-9]\+;/simple_scheduler $MAXSESSION;/" $CONF
echo "OK"

prog=shiny-server
execute=$(which $prog)
logfile=/var/log/${prog}.log
lockfile=/var/run/${prog}.pid

echo "Starting shiny-server ... " 
$prog --daemon "--pidfile=$lockfile" >> $logfile

