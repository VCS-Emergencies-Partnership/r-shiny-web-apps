ARG R_VERSION=4.0.2
FROM rocker/shiny:${R_VERSION}

ARG SRV_DIR="/srv/shiny-server"

# Install application-specific dependencies
RUN apt-get update && apt-get install -y \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libssl-dev \
    libudunits2-dev \
    libxml2-dev

# {renv} version to use
ENV RENV_VERSION 0.13.2
# Install {remotes} to install {renv} from github
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
# Install {renv}
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

WORKDIR ${SRV_DIR}
COPY renv.lock renv.lock
RUN R -e 'renv::restore()'

# Copy application code and data into shiny dir
COPY . ${SRV_DIR}

# Use custom shiny-server config
COPY ./shiny-customized.config /etc/shiny-server/shiny-server.conf
USER shiny
