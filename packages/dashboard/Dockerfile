ARG R_VERSION=4.0.2
FROM rocker/shiny:${R_VERSION}

ARG SRV_DIR="/srv/shiny-server/"

# Install application-specific dependencies and then
# clean up the apt cache to reduce the image size
RUN apt-get update && apt-get install -y --no-install-recommends \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libssl-dev \
    libudunits2-dev \
    libxml2-dev \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*

RUN install2.r --error packrat

WORKDIR ${SRV_DIR}
COPY packrat/packrat.lock ./packrat/
RUN Rscript -e "packrat::init(infer.dependencies = FALSE, \
                              enter = TRUE, \
                              restart = FALSE); \
                packrat::restore()"

# Use custom shiny-server config
COPY ./shiny-customized.config /etc/shiny-server/shiny-server.conf

# Copy application code and data into shiny dir
COPY *.R google-analytics.html shiny-server.sh styles.css .Rprofile ${SRV_DIR}

COPY data ${SRV_DIR}/data
COPY www ${SRV_DIR}/www
