FROM ubuntu:18.04

MAINTAINER "Daniel Jacob" daniel.jacob@u-bordeaux.fr

ENV DEBIAN_FRONTEND=noninteractive \
    TZ=Europe/Paris
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

RUN apt-get update && apt-get install -y \
    sudo ca-certificates wget curl \
    libcurl4-gnutls-dev libcairo2-dev libv8-dev libssl-dev \
    gdebi-core \
    software-properties-common && \
  # See https://www.digitalocean.com/community/tutorials/how-to-install-r-on-ubuntu-18-04'
  #     https://rtask.thinkr.fr/fr/installation-de-r-3-5-sur-ubuntu-18-04-lts-et-astuces-pour-les-packages-de-cartographie/
    sh -c 'echo "deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran35/" >> /etc/apt/sources.list' && \
    apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9 && \
    add-apt-repository ppa:marutter/c2d4u3.5 && \
  # Install R / ...
    apt-get update && apt-get install -y \
    r-recommended r-base-dev r-cran-rcurl r-cran-base64enc r-cran-xml r-cran-stringr \
    r-cran-rcpp r-cran-biocmanager r-cran-mime r-cran-inline r-cran-docopt r-cran-remotes \
    r-cran-httpuv r-cran-xtable r-cran-htmlwidgets r-cran-jsonlite r-cran-htmltools \
    r-cran-igraph r-cran-magrittr r-cran-ellipse r-cran-whisker r-cran-reshape2 \
    r-cran-markdown  r-cran-plotly r-cran-ggplot2 r-cran-moments r-cran-ggpubr r-cran-ggdendro \
    r-bioc-limma r-bioc-impute r-bioc-pcamethods r-cran-rcppeigen r-cran-rcppparallel \
    r-cran-shiny r-cran-shinybs r-cran-shinydashboard r-cran-shinyjs r-cran-dt && \
  # locale setting
    apt-get update && apt-get install -y locales && \
    echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen && \
    locale-gen en_US.utf8 && \
    /usr/sbin/update-locale LANG=en_US.UTF-8 && \
    apt-get clean && apt-get purge && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

## Configure default locale, see https://github.com/rocker-org/rocker/issues/19
RUN echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
    && locale-gen en_US.utf8 \
    && /usr/sbin/update-locale LANG=en_US.UTF-8

ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8

# Download and install shiny server
RUN wget --no-verbose http://download3.rstudio.org/ubuntu-12.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "http://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb

# R packages installed in /usr/local/lib/R/site-library/
RUN R -e "install.packages(c('JADE'), repos='http://cran.rstudio.com/')" && \
    R -e "install.packages('https://cran.r-project.org/src/contrib/Archive/networkD3/networkD3_0.2.13.tar.gz', repos=NULL)" && \
    R -e "remotes::install_github('wt2015-github/FastGGM');"

ADD ./ /srv/shiny-server

RUN rm -rf /var/lib/apt/lists/* && \ 
    cp /srv/shiny-server/launch-server.sh /usr/local/bin && \
    chmod 755 /usr/local/bin/launch-server.sh


EXPOSE 3838

CMD ["/usr/local/bin/launch-server.sh"]

