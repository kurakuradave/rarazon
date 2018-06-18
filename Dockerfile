FROM ubuntu:16.04
RUN echo "deb http://cran.rstudio.com/bin/linux/ubuntu xenial/" | tee -a /etc/apt/sources.list &&\
     gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9 &&\
     apt-get update &&\
     apt-get install r-base r-base-dev


