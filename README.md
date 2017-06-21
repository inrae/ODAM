# ODAM
### Open Data, Access and Mining

Give an open access to your data and make them ready to be mined - A data explorer as bonus

See [presentation](http://fr.slideshare.net/danieljacob771282/odam-open-data-access-and-mining) on fr.slideshare.net

------
**Maintainer**: Daniel Jacob UMR 1332 BAP – Metabolism Group Bordeaux Metabolomics Facility MetaboHUB 2015


### Installation

Requirements:
  * a recent Linux OS that support Docker (see https://www.docker.com/)


From Github, Clone the repository

```
    $ git clone https://github.com/djacob65/ODAM.git
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
  * To prepare your own data subsets, see the [tutorial on metadata files](https://github.com/djacob65/ODAM/blob/master/doc/tutorial_on_metadata_files.pdf)

### Get the complete FRIM dataset as an example

```
    $ wget https://zenodo.org/record/154041/files/frim1.zip
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
    $ curl "http://localhost:8080/tsv/your_dataset_name
```

In your Web browser, you can launch the Data Explorer connected to your dataset with the URL http://localhost/?ds=your_dataset_name (depending on the GETDATA_URL_PROXY and DATAEXPLORER_PORT settings in the ./odam.sh file ).

### Future improvements
    * A web page allowing users to annotate the attributes with ontologies (based on BioPortal API) 
    * Linked Data format / RDF export

### License
GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 - See the included LICENSE file.
