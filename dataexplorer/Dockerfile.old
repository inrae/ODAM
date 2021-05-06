FROM ubuntu:14.04

MAINTAINER "Daniel Jacob" daniel.jacob@u-bordeaux.fr

RUN apt-get update -qq \
    && apt-get install -y \
    ca-certificates \
    sudo \
    wget \
    curl \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libv8-dev \
    gdebi-core \
    software-properties-common

# See https://www.digitalocean.com/community/tutorials/how-to-set-up-r-on-ubuntu-14-04
RUN sh -c 'echo "deb http://cran.rstudio.com/bin/linux/ubuntu trusty/" >> /etc/apt/sources.list' && \
    gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9 && \
    gpg -a --export E084DAB9 | sudo apt-key add -

# Install R in /usr/lib/R/(site-)library/
RUN apt-get update \
    && apt-get install -y r-recommended \
    r-cran-rcurl \
    r-cran-base64enc \
    r-cran-stringr

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
RUN R -e "install.packages( c('httpuv', 'mime', 'jsonlite', 'xtable', 'htmltools', 'R6', 'V8'), repos='http://cran.rstudio.com/')"
RUN R -e "install.packages( c('moments', 'JADE', 'ellipse', 'whisker'), repos='http://cran.rstudio.com/')"
RUN R -e "install.packages( c('shiny', 'shinyBS', 'shinydashboard', 'shinyjs', 'DT'), repos='http://cran.rstudio.com/')"

# 'networkD3'
RUN R -e "install.packages( c('htmlwidgets', 'igraph', 'magrittr'), repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('https://cran.r-project.org/src/contrib/Archive/networkD3/networkD3_0.2.13.tar.gz', repos=NULL)"

# https://plot.ly/r/shiny-tutorial/ & https://plot.ly/r/ & https://plot.ly/ggplot2/
RUN apt-get install -y libssl-dev \
    && R -e "install.packages( c('plotly', 'ggplot2'), repos='http://cran.rstudio.com/')"

RUN R -e "install.packages(c('markdown', 'ggpubr', 'ggdendro'), repos='http://cran.rstudio.com/')"

RUN R -e "install.packages('RcppParallel', repos='http://cran.rstudio.com/')" && \
    wget --no-verbose http://www.pitt.edu/~wec47/Files/FastGGM.tar.gz -O "FastGGM.tar.gz" && \
    R -e "install.packages(pkgs = 'FastGGM.tar.gz', repos = NULL, type = 'source')" && \
    rm -f FastGGM.tar.gz

RUN R -e "source('http://bioconductor.org/biocLite.R'); biocLite(c('limma','impute','pcaMethods'))"

ADD ./ /srv/shiny-server

RUN rm -rf /var/lib/apt/lists/* && \ 
    cp /srv/shiny-server/launch-server.sh /usr/local/bin && \
    chmod 755 /usr/local/bin/launch-server.sh


EXPOSE 3838

CMD ["/usr/local/bin/launch-server.sh"]

