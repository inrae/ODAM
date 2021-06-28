#!/bin/bash
MYDIR=`dirname $0` && [ ! `echo "$0" | grep '^\/'` ] && MYDIR=`pwd`/$MYDIR

# Development mode: 0 => use only source files within the docker images; 1 => use local source files
DEV=0

# URL Proxy for the "getdata" web-services
GETDATA_URL_PROXY=http://my_host.com:8081/

# Path root of the data repository
GETDATA_DATAREPOS=/opt/DataRepos

# GetData Container
GETDATA_PORT=8081
GETDATA_IMAGE=docker.io/odam/getdata
GETDATA_CONTAINER=gdata

# Dataexplorer Container
DATAEXPLORER_PORT=8080
DATAEXPLORER_IMAGE=docker.io/odam/dataexplorer
DATAEXPLORER_CONTAINER=dataexplorer

CMD=$1

# If you use a named volume, (assumes that your docker version >= 1.9)
# - First you have to create the /opt/data volume 
# sudo docker create -v /opt/data --name odam_data_volume ubuntu
# - Second, uncomment the line below, and 
#   comment the line with 'VOLS' specified further with a local directory .
#VOLS="--volumes-from odam_data_volume"

# If you use a local directory, first you have to create the /opt/data directory
VOLS="-v $GETDATA_DATAREPOS:/opt/data"

usage() { echo "usage: sh $0 start|stop|restart|ps|build|push|pull";  exit 1; }

case "$CMD" in
   start) 
        # run getData
        VOLSRC=''; [ $DEV -eq 1 ] && VOLSRC="-v $MYDIR/getData/www:/var/www/html"
        sudo docker run -d -e "GETDATA_URL_PROXY="$GETDATA_URL_PROXY $VOLS  $VOLSRC -p $GETDATA_PORT:80 \
                    --name $GETDATA_CONTAINER $GETDATA_IMAGE
        
        # run dataexplorer
        VOLSRC=''; [ $DEV -eq 1 ] && VOLSRC="-v $MYDIR"/dataexplorer":/srv/shiny-server/"
        sudo docker run -d -e "GETDATA_URL_PROXY="$GETDATA_URL_PROXY $VOLSRC -p $DATAEXPLORER_PORT:3838 \
                    --name $DATAEXPLORER_CONTAINER $DATAEXPLORER_IMAGE
        
        # show logs
        sudo docker logs $GETDATA_CONTAINER
        sudo docker logs $DATAEXPLORER_CONTAINER
        ;;
   stop)
        sudo docker rm -f $GETDATA_CONTAINER $DATAEXPLORER_CONTAINER
        ;;
   restart)
        ( sh $0 stop ; sh $0 start )
        ;;
   ps)
        sudo docker ps | head -1
        sudo docker ps | grep "odam/"
        ;;
   build)
        ( cd $MYDIR/getdata;      sudo docker build -t $GETDATA_IMAGE . )
        ( cd $MYDIR/dataexplorer; sudo docker build -t $DATAEXPLORER_IMAGE . )
        ;;
   pull)
        sudo docker pull $GETDATA_IMAGE
        sudo docker pull $DATAEXPLORER_IMAGE
        ;;
   *) usage
      exit 2
esac

