#! /bin/bash
if [ ! -d "/tmp/backend/" ]; then mkdir /tmp/backend/; fi
if [ ! -d "/tmp/frontend/" ]; then mkdir /tmp/frontend/; fi
rm -rf /tmp/backend/*
rm -rf /tmp/frontend/*
cp -f -t /tmp/backend/ cli_backend/data/*
cp -f -t /tmp/frontend/ cli_frontend/data/*