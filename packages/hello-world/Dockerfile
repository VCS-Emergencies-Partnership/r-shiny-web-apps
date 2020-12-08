FROM rocker/shiny:3.6.3
ARG SRC_DIR="/srv/shiny-server"
COPY . $SRC_DIR
WORKDIR $SRC_DIR
RUN install2.r --error \
  --skipinstalled \
  $(cat $SRC_DIR/deps.txt)
USER shiny
