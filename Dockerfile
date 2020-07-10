FROM rocker/shiny

RUN apt-get update; apt-get install -y inotify-tools; apt-get install -y libssl-dev

#ADD . /app
#WORKDIR /app
RUN Rscript ./newerhoods/setup.R

EXPOSE 8080

COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

## assume shiny app is in build folder /shiny
COPY ./newerhoods/ /srv/shiny-server/shiny/

#RUN chmod u+x entrypoint.sh
#ENTRYPOINT /app/entrypoint.sh


