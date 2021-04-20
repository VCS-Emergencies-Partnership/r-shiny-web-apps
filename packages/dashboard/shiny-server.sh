#!/bin/bash

echo HELLO WE ARE RUNNING

mkdir -p /var/log/shiny-server
chown shiny.shiny /var/log/shiny-server /var/lib/shiny-server/

#env > /home/shiny/.Renviron
cp /srv/shiny-server/.Renviron /home/shiny/.Renviron
chown shiny.shiny /home/shiny/.Renviron

if [ "$APPLICATION_LOGS_TO_STDOUT" != "false" ];
	then
# push the "real" application logs to stdout with xtail in detached mode
exec xtail /var/log/shiny-server/ &
	fi

#start shiny server
exec shiny-server 2>&1
