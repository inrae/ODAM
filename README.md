![Logo](odam-logo.png)

[![GitHub release](https://img.shields.io/github/release/inrae/ODAM.svg)](https://GitHub.com/inrae/ODAM/releases/)

# ODAM
### Open Data, Access and Mining

ODAM is an Experiment Data Table Management System (EDTMS) that gives you an open access to your data and make them ready to be mined - A data explorer as bonus

For more information, see [ODAM: Deployment and User's Guide](https://inrae.github.io/ODAM/)

------
**Maintainer**: Daniel Jacob - INRAE - UMR 1332 BFP (2017-2021)


### Installation

Requirements:
  * a recent Linux OS that support Docker (see https://www.docker.com/)


From Github, Clone the repository

```
    $ git clone https://github.com/inrae/ODAM.git
```
   

Then `cd` to your clone path, and build the docker images (may take a while depending on your network speed and the traffic)

```
    $ cd ODAM
    $ sh ./odam.sh build
```

Or you may prefer getting directly the docker images from the official [docker repository](https://hub.docker.com/r/odam/getdata/)

```
    $ cd ODAM
    $ sh ./odam.sh pull
```

### Data Repository

  * Create a data repository (i.e. a directory) anywhere your ODAM installation can access it either by a symbolic link or a mount point to a local data directory.

```
    $ mkdir /opt/DataRepos
```

  * Create / add a dataset into your data repository (see [presentation](http://fr.slideshare.net/danieljacob771282/odam-open-data-access-and-mining))
  * To prepare your own data subsets, see the [tutorial on metadata files](https://github.com/INRA/ODAM/blob/master/doc/tutorial_on_metadata_files.pdf)

### Get the complete FRIM dataset as an example

```
    $ cd /opt/DataRepos
    $ wget https://data.inrae.fr/api/access/datafile/:persistentId/?persistentId=doi:10.15454/95JUTK/P0ZJMJ -O frim1.zip
    $ unzip frim1.zip
    $ rm -f frim1.zip
```

### Configuration

Edit the ./odam.sh file , change the GETDATA_URL_PROXY, GETDATA_DATAREPOS, GETDATA_PORT and DATAEXPLORER_PORT  parameters according to your local configuration

### Usage

```
     Usage: sh ./odam.sh COMMAND

     Commands:
        pull            pull the docker images from the official docker  repository
        build           build the docker images of the web-service 'getdata' and the web application 'dataexplorer'
        start           start the docker containers from their docker image
        stop            stop the docker containers
        ps              view the status of docker containers
```

Then, start the docker containers
```
    $ sh ./odam start
```

Normally if everything is ok, you can access your data via web-services. You can test for example with the command curl (depending on the GETDATA_URL_PROXY and GETDATA_PORT settings in the ./odam.sh file ):
```
    $ curl "http://my_host.com:8081/query/<your_dataset_name>
```
Or test your getData API through the web swagger UI (go to [API](https://github.com/INRA/ODAM/tree/master/API) folder)

In your Web browser, you can launch the Data Explorer connected to your dataset with the URL http://my_host.com:8080/?ds=your_dataset_name (depending on the GETDATA_URL_PROXY and DATAEXPLORER_PORT settings in the ./odam.sh file ).


### NGINX configuration (Linux)

For avanced users: In case you would like to use a proxy server, the better is to install and set NGINX, an HTTP and reverse proxy server.

In the /etc/nginx/conf.d/my-site.conf, you should add two 'location' sections as shown below:

```
server {
    listen 80 default;
    server_name $host;

    ...

   location /getdata/ {
        proxy_pass http://$host:8081;
    }

    location /dataexplorer/ {
        proxy_pass http://$host:8080;
    }

    ...

}
```

In this way, you can use the URL http://my_host.com/getdata/... for API and the URL http://my_host.com/dataexplorer/... for the data explorer


### Future improvements
    * A web page allowing users to annotate the attributes with ontologies (based on BioPortal API) 
    * Linked Data format / RDF export
    * See https://inrae.github.io/ODAM/todo/

### License
GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 - See the included LICENSE file.
