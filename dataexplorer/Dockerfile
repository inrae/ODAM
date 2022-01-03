FROM ubuntu:18.04

MAINTAINER "Daniel Jacob" daniel.jacob@u-bordeaux.fr

ENV DEBIAN_FRONTEND=noninteractive \
    TZ=Europe/Paris
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

RUN apt-get update && apt-get install -y \
    sudo ca-certificates wget curl git vim ed unzip\
    libcurl4-gnutls-dev libcairo2-dev libv8-dev libssl-dev \
    openssl gdebi-core \
    build-essential  software-properties-common \
 # Install Repositories
 && sh -c 'echo "deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran40/" >> /etc/apt/sources.list' \
 && apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9 \
 && add-apt-repository ppa:c2d4u.team/c2d4u4.0+ \
  # Install R / ...
 && apt-get update && apt-get install -y \
    r-recommended r-base-dev r-cran-rcurl r-cran-base64enc r-cran-xml r-cran-stringr \
    r-cran-rcpp r-cran-biocmanager r-cran-mime r-cran-inline r-cran-docopt \
    r-cran-httpuv r-cran-xtable r-cran-htmlwidgets r-cran-jsonlite r-cran-htmltools \
    r-cran-igraph r-cran-magrittr r-cran-ellipse r-cran-whisker r-cran-reshape2 \
    r-cran-markdown  r-cran-plotly r-cran-ggplot2 r-cran-remotes r-cran-moments r-cran-ks \
    r-bioc-limma r-bioc-impute r-bioc-pcamethods r-cran-rcppeigen r-cran-rcppparallel \
    r-cran-shiny r-cran-shinybs r-cran-shinydashboard r-cran-shinyjs r-cran-dt \
    r-cran-knitr r-cran-clipr r-cran-rematch r-cran-readr r-cran-car r-cran-nloptr r-cran-clue \
    r-cran-polynom r-cran-lme4 r-cran-haven r-cran-ggpubr r-cran-ggdendro r-cran-fdrtool \
    r-cran-plyr r-cran-rtsne r-cran-shinycssloaders r-cran-doparallel \
  # locale setting
 && apt-get update && apt-get install -y locales \
 && echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
 && locale-gen en_US.utf8 \
 && /usr/sbin/update-locale LANG=en_US.UTF-8 \
 && apt-get clean autoclean purge \
 && apt-get autoremove --yes \
 && rm -rf /var/lib/{apt,dpkg,cache,log}/ \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8

# Download and install shiny server
RUN wget --no-verbose http://download3.rstudio.org/ubuntu-12.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "http://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb

# R packages installed in /usr/local/lib/R/site-library/
RUN R -e "install.packages(c('svglite', 'JADE'), repos='http://cran.rstudio.com/')" \
 && R -e "install.packages('https://cran.r-project.org/src/contrib/Archive/networkD3/networkD3_0.2.13.tar.gz', repos=NULL)" \
 && R -e "remotes::install_github('wt2015-github/FastGGM')" \
 && R -e "remotes::install_github('jokergoo/ComplexHeatmap')"
 
RUN R -e "install.packages('ggvenn', repos='http://cran.rstudio.com/')"

ADD ./ /srv/shiny-server

RUN rm -rf /var/lib/apt/lists/* && \ 
    cp /srv/shiny-server/conf/shiny-server.conf /etc/shiny-server/shiny-server.conf && \
    cp /srv/shiny-server/conf/launch-server.sh /usr/local/bin && \
    chmod 755 /usr/local/bin/launch-server.sh


EXPOSE 3838

CMD ["/usr/local/bin/launch-server.sh"]

