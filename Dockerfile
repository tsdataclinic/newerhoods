FROM rocker/geospatial:3.5.0



RUN apt-get update; apt-get install -y inotify-tools

ADD . /app
WORKDIR /app
RUN Rscript ./newerhoods/setup.R

RUN chmod u+x entrypoint.sh
ENTRYPOINT /app/entrypoint.sh
