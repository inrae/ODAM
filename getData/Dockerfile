FROM php:5.6-apache

RUN apt-get update -qq && apt-get dist-upgrade -y

# See https://github.com/docker-library/docs/tree/master/php

RUN apt-get install -y \
    python-minimal \
    vim \
    libfreetype6-dev \
    libjpeg62-turbo-dev \
    libmcrypt-dev \
    libpng12-dev \
    && docker-php-ext-install iconv mcrypt \
    && docker-php-ext-configure gd --with-freetype-dir=/usr/include/ --with-jpeg-dir=/usr/include/ \
    && docker-php-ext-install gd

RUN curl -sL https://deb.nodesource.com/setup_6.x | bash - \
    && apt-get install -y nodejs build-essential \
    && npm install -g csvtojson

ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8

COPY ./www/bin/q /usr/bin/q
RUN chmod 755 /usr/bin/q

RUN mkdir -p /opt/data
VOLUME /opt/data

ADD ./www /var/www/html

WORKDIR /var/www/html

EXPOSE 80

CMD ["apache2-foreground"]
