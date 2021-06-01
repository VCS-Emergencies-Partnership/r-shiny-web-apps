ARG R_VERSION=4.0.2
FROM rocker/shiny:${R_VERSION}

# SSH access to the container is configured to only allow access from within
# the bridge network of a private virtual network, and is not accessible to an
# attacker on the internet.

# Install OpenSSH and set the password for root to "Docker!"
RUN apt-get update && apt-get install -y --no-install-recommends \
    openssh-server \
    && echo "root:Docker!" | chpasswd 

COPY sshd_config /etc/ssh/

ARG SRV_DIR="/srv/shiny-server"

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

# Install {packrat} for managing our R environment
RUN install2.r --error packrat

# Move to srv directory to ensure packrat
# libs are created in the correct location
WORKDIR ${SRV_DIR}
COPY packrat/packrat.lock ./packrat/
# Install dependencies as specified by the packrat.lock file
RUN Rscript -e "packrat::init(infer.dependencies = FALSE, \
                              enter = TRUE, \
                              restart = FALSE); \
                packrat::restore()"

# Use custom shiny-server config
COPY ./shiny-customized.config /etc/shiny-server/shiny-server.conf

# Copy application code and data into shiny dir
COPY cookie_consent_v2.html \
     *.R \
     .Renviron* \
     google-analytics.html \
     shiny-server.sh \
     styles.css \
     .Rprofile \
    ${SRV_DIR}/

# Copy data into container (in the future to be loaded at runtime)
COPY data ${SRV_DIR}/data
# Copy styling and images thr
COPY www ${SRV_DIR}/www

# Open port 2222 for SSH access
EXPOSE 80 2222
