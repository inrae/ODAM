#!/bin/bash
#
# rsync_data.sh
# Usage:
# sh ./rsync_data.sh [dataset]
#     if [dataset] is specified then only this dataset will be synchronized remotely
#     otherwise all datasets will be synchronized remotely
#
MYDIR=`dirname $0` && [ ! `echo "$0" | grep '^\/'` ] && MYDIR=`pwd`/$MYDIR

ROOT_LOCAL=/cygdrive/c/DATA/ODAM
SERVER=root@odamserver
ROOT_DIST=/opt/dockerApps/ODAM

APP=
[ $# -gt 0 ] && APP=$1

RSYNC="rsync -av  --delete-after --include '*/'"

[ -f $MYDIR/ODAM_exclude.txt ] && EXCLUDE="--exclude-from=ODAM_exclude.txt"

NAME=data
[ $# -gt 0 ] && NAME=data/$APP
[ ! -d $ROOT_LOCAL/$NAME ] && echo "ERROR: Dataset $APP does not exist !!" && exit 1


echo "RSYNC ..."
$RSYNC $EXCLUDE $ROOT_LOCAL/$NAME/ $SERVER:$ROOT_DIST/$NAME/ 2>&1 | grep -v -E "^gid "
echo ""

echo -n "CHMOD ..."
ssh $SERVER "chown -R root.root $ROOT_DIST/$NAME; find $ROOT_DIST/$NAME -type d -exec chmod 755 {} \; ; find $ROOT_DIST/$NAME -type f -exec chmod 664 {} \;"
echo "OK"


