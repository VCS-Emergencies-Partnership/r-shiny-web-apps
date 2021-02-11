FROM rocker/shiny:3.6.3
ARG SRC_DIR="/srv/shiny-server"
COPY . $SRC_DIR
WORKDIR $SRC_DIR
RUN apt-get update \
  && apt-get install -y \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libssl-dev \
    libudunits2-dev \
    libxml2-dev \
  && install2.r --error \
  --skipinstalled \
  $(cat $SRC_DIR/deps.txt)
USER shiny
