#!/bin/sh

PORT=${2-3000}

start () {
  R -e "source('newerhoods/settings_local.R'); shiny::runApp('newerhoods',port=$PORT, host='0.0.0.0')" &
  PID=$!
}

start

inotifywait -mr newerhoods --format '%e %f' \
  -e modify -e delete -e move -e create \
  | while read event file; do

  echo $event $file

  kill $PID
  start

done
