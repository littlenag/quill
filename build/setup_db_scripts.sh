#!/usr/bin/env bash

export POSTGRES_SCRIPT=quill-sql/src/test/sql/postgres-schema.sql

function get_host() {
    if [ -z "$1" ]; then
        echo "127.0.0.1"
    else
        echo "$1"
    fi
}
# usage: setup_x <script>

function setup_postgres() {
    host=$(get_host $2)
    echo "Waiting for Postgres"
    until psql -h $2 -U postgres -c "select 1" &> /dev/null; do
        sleep 5;
    done
    echo "Connected to Postgres"

    psql -h $2 -U postgres -c "CREATE DATABASE spill_test"
    psql -h $2 -U postgres -d spill_test -a -q -f $1
}

export -f setup_postgres