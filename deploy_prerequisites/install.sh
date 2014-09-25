#! /bin/bash
# /etc/backend
rm -rf /etc/backend
mkdir /etc/backend
cp -t /etc/backend cli_backend_data/*
chmod 0444 /etc/backend/*
# /etc/frontend
rm -rf /etc/frontend
mkdir /etc/frontend
cp -t /etc/frontend cli_frontend_data/*
chmod 0444 /etc/frontend/*
# backend libs
rm -rf /usr/local/lib/backend
mkdir /usr/local/lib/backend
cp -t /usr/local/lib/backend cli_backend_ebin/*
rm /usr/local/lib/backend/cli_backend_start.sh
chmod 0444 /usr/local/lib/backend/*
# frontend libs
rm -rf /usr/local/lib/frontend
mkdir /usr/local/lib/frontend
cp -t /usr/local/lib/frontend cli_frontend_ebin/*
rm /usr/local/lib/frontend/cli_frontend_start.sh
chmod 0444 /usr/local/lib/frontend/*
# binaries
rm /usr/local/bin/cli_backend_start.sh
rm /usr/local/bin/cli_frontend_start.sh
rm /usr/local/bin/cli_terminal
cp cli_backend_ebin/cli_backend_start.sh /usr/local/bin
cp cli_frontend_ebin/cli_frontend_start.sh /usr/local/bin
cp cli_terminal_bin/cli_terminal /usr/local/bin
chmod 0755 /usr/local/bin/cli_backend_start.sh
chmod 0755 /usr/local/bin/cli_frontend_start.sh
chmod 0555 /usr/local/bin/cli_terminal
# cli_user
userdel cli_user
useradd -s /usr/local/bin/cli_terminal cli_user
# set passwd, e.g. password=1
# passwd cli_user