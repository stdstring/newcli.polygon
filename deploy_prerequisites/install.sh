#! /bin/bash
# /etc/cli_service
rm -rf /etc/cli_service
mkdir /etc/cli_service
cp -t /etc/cli_service cli_service_data/*
chmod 0444 /etc/cli_service/*
# /etc/cli_terminal
rm -rf /etc/cli_terminal
mkdir /etc/cli_terminal
cp -t /etc/cli_terminal cli_terminal_data/*
chmod 0444 /etc/cli_terminal/*
# cli_service libs
rm -rf /usr/local/lib/cli_service
mkdir /usr/local/lib/cli_service
cp -t /usr/local/lib/cli_service cli_service_ebin/*
rm /usr/local/lib/cli_service/cli_service_start.sh
chmod 0444 /usr/local/lib/cli_service/*
# binaries
rm -f /usr/local/bin/cli_service_start.sh
rm -f /usr/local/bin/cli_terminal
cp cli_service_ebin/cli_service_start.sh /usr/local/bin
cp cli_terminal_bin/cli_terminal /usr/local/bin
chmod 0755 /usr/local/bin/cli_service_start.sh
chmod 0555 /usr/local/bin/cli_terminal
# cli_user
userdel cli_user
useradd -s /usr/local/bin/cli_terminal cli_user
# set passwd, e.g. password=1
passwd cli_user