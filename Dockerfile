# Specify inheritance
FROM ubuntu:18.04

# Turn off interactive mode (so e.g. R installation does not ask you for your location)
ENV DEBIAN_FRONTEND noninteractive

# Update package database for OS
RUN apt-get update

# Configure package installation
RUN echo 'APT::Get::Install-Recommends "false";' >> /etc/apt/apt.conf

#################################
# R
#################################

# Create .Rprofile file to specify package repository, define installOrQuit function, set number of allowed CPUs
RUN echo 'options(Ncpus=4, repos=structure(c(CRAN="https://cloud.r-project.org")))' > ~/.Rprofile
RUN echo 'installOrQuit <- function(p) {tryCatch(install.packages(p), warning=function(e){q(status=1)})}' >> ~/.Rprofile

# Install base R with version
#ARG R_VERSION=4.1.2
#RUN apt-get install -y --no-install-recommends \
#  r-base-core=$R_VERSION \
#  r-base-html=$R_VERSION \
#  r-doc-html=$R_VERSION

# Install base r (newest version)
RUN apt install -y r-base

# Install specified packages
RUN Rscript -e "installOrQuit(c('here', 'data.table'))"

#################################
# Python
#################################

ARG PYTHON_VERSION=3.5
RUN apt-get update
RUN apt-get install python$PYTHON_VERSION
