#!/bin/bash

set -e

# Apache gets grumpy about PID files pre-existing
rm -f /var/run/apache2/apache2.pid

echo "Starting apache2 server ..."
exec apache2 -DFOREGROUND

