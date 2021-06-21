#!/bin/bash

set -e

# Apache gets grumpy about PID files pre-existing
rm -f /var/run/apache2/apache2.pid

echo "Starting apache2 server ..."
/usr/sbin/apache2

# Apache2 having started with the default sqlite3 dynamic library,
# (in fact it's PHP that requires this library)
# we can now change this library with a new version 
# (limitation of the number of columns set to its maximum)
mv /usr/share/lib/libsqlite3.so.* /usr/lib/x86_64-linux-gnu/

sleep infinity
