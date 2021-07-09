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

  * Create / add a dataset into your data repository (see [presentation](https://inrae.github.io/ODAM/pdf/FAIR_and_DataLife_DJ_Oct2019.pdf))
  * To prepare your own data subsets, see the [ODAM: Data collection and preparation](https://inrae.github.io/ODAM/data-type/)

### Get the complete FRIM dataset as an example

```
    $ cd /opt/DataRepos
    $ wget https://data.inrae.fr/api/access/datafile/:persistentId/?persistentId=doi:10.15454/95JUTK/P0ZJMJ -O frim1.zip
    $ unzip frim1.zip
    $ rm -f frim1.zip
```

### Configuration

**Edit the ./odam.sh file** , change the **GETDATA_URL_PROXY**, **GETDATA_DATAREPOS**, **GETDATA_PORT** and **DATAEXPLORER_PORT**  parameters according to your local configuration

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
    $ sh ./odam.sh start
```

Normally if everything is ok, you can access your data via web-services. You can test for example with the command curl (depending on the GETDATA_URL_PROXY and GETDATA_PORT settings in the ./odam.sh file ):
```
    $ curl "http://myhost.org:8081/query/<your_dataset_name>"
```
Or test your getData API through the web swagger UI (go to [API](https://github.com/INRA/ODAM/tree/master/API) folder)

In your Web browser, you can launch the Data Explorer connected to your dataset with the URL http://myhost.org:8080/?ds=your_dataset_name (depending on the GETDATA_URL_PROXY and DATAEXPLORER_PORT settings in the ./odam.sh file ).


### NGINX configuration (Linux)

For avanced users: In case you would like to use a proxy server, the better is to install and set NGINX, an HTTP and reverse proxy server.

In the /etc/nginx/conf.d/my-site.conf, you should add two 'location' sections as shown below:

```
server {
    listen 80 default;
    server_name $host;

    ...

   location /getdata/ {
        proxy_set_header Host $host;
        proxy_set_header X-Real-Ip $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_pass http://localhost:8081;
    }

    location /dataexplorer/ {
        proxy_pass http://localhost:8080;
    }

    ...

}
```

In this way, you can use the URL http://myhost.org/getdata/... for API and the URL http://myhost.org/dataexplorer/... for the data explorer


### Future improvements
    * A web page allowing users to annotate the attributes with ontologies (based on AgroPortal/BioPortal API) 
    * Linked Data format / RDF export
    * See https://inrae.github.io/ODAM/todo/


### Publication:

Daniel Jacob, Romain David, Sophie Aubin, Yves Gibon (2020) Making experimental data tables in the life sciences more FAIR: a pragmatic approach, GigaScience, Volume 9, Issue 12, December 2020, [doi:10.1093/gigascience/giaa144](http://dx.doi.org/10.1093/gigascience/giaa144)


### Funded by:

* INRAE UMR 1332 BFP, Bordeaux Metabolomics Facility


### License

GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 - See the included LICENSE file.
