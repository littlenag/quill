#!/usr/bin/env bash

set -e

time docker-compose up -d cassandra sqlserver orientdb

# import setup functions
. build/setup_db_scripts.sh

# run setup scripts for local databases
time setup_postgres $POSTGRES_SCRIPT 127.0.0.1

function send_script() {
  docker cp $2 "$(docker-compose ps -q $1)":/$3
}

echo "Databases are ready!"