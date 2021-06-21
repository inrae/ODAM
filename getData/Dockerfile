FROM php:7.4.18-apache

RUN apt-get update -qq && apt-get dist-upgrade -y

# See https://github.com/docker-library/docs/tree/master/php

RUN apt-get install -y \
# Install some packages
    python-minimal vim \
    libfreetype6-dev libjpeg62-turbo-dev libmcrypt-dev libpng-dev \
    software-properties-common \
# Install & configure mcrypt & gd for PHP
 && pecl install mcrypt \
 && docker-php-ext-enable mcrypt  \
 && docker-php-ext-configure gd --with-freetype --with-jpeg \
 && docker-php-ext-install gd \
 && rm -f /usr/src/php.tar.* \
# Install & configure sqlite3 without column limit
 && apt-get install -y sqlite3 libsqlite3-dev tclsh \
 && cd /home \
 && curl https://www.sqlite.org/src/tarball/sqlite.tar.gz -o sqlite.tar.gz \
 && tar xvzf ./sqlite.tar.gz \
 && cd sqlite \
 && sed -i -e "s/SQLITE_MAX_COLUMN 2000/SQLITE_MAX_COLUMN 32767/" ./src/sqliteLimit.h \
 && ./configure && make && make install \
 && rm -f /usr/bin/sqlite3 && rm -f /home/sqlite.tar.gz && rm -rf /home/sqlite \
 && update-alternatives --install /usr/bin/sqlite3 sqlite3 /usr/local/bin/sqlite3 1 \
 && mkdir -p /usr/share/lib && mv /usr/local/lib/libsqlite3.so.* /usr/share/lib/ \
 && rm -f /usr/local/lib/libsqlite3.*

# Install nodejs && csvtojson
RUN curl -sL https://deb.nodesource.com/setup_16.x | bash - \
 && apt-get install -y nodejs build-essential \
 && npm install -g csvtojson

# Install q-text-as-data
RUN cd /home \
 && curl -L https://github.com/harelba/q/releases/download/2.0.19/q-text-as-data_2.0.19-2_amd64.deb -o q-text-as-data.deb \
 && dpkg -i q-text-as-data.deb \
 && rm -f q-text-as-data.deb

# Environment for Apache
ENV LC_ALL=en_US.UTF-8 \
    LANG=en_US.UTF-8 \
    APACHE_RUN_USER=www-data \
    APACHE_RUN_GROUP=www-data \
    APACHE_LOG_DIR=/var/log/apache2 \
    APACHE_PID_FILE=/var/run/apache2.pid \
    APACHE_RUN_DIR=/var/run/apache2 \
    APACHE_LOCK_DIR=/var/lock/apache2 \
    APACHE_SERVERADMIN=admin@localhost \
    APACHE_SERVERNAME=localhost \
    APACHE_SERVERALIAS=docker.localhost \
    APACHE_DOCUMENTROOT=/var/www/html

# Copy configuration and source files 
COPY ./php.ini /usr/local/etc/php/php.ini
COPY ./launch-server.sh /usr/bin/launch-server.sh
ADD ./www /var/www/html

# Final touch
RUN chmod -R 777 /var/www/html \
 && chmod 755 /usr/bin/q \
 && chmod 755 /usr/bin/launch-server.sh \
 && mkdir -p /opt/data \
 && echo "alias ll='ls -al'" >> /root/.bashrc
 
VOLUME /opt/data

WORKDIR /var/www/html

EXPOSE 80

CMD ["/usr/bin/launch-server.sh"]
