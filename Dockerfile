FROM ubuntu:16.04
RUN echo "deb http://cran.rstudio.com/bin/linux/ubuntu xenial/" | tee -a /etc/apt/sources.list &&\
    gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9 &&\
    apt-get update &&\
    apt-get install -y --allow-unauthenticated \ 
        libcurl4-openssl-dev \
        libxml2 \
        libxml2-dev \
        libssl-dev \
        libudunits2-dev \
        r-base \
        r-base-dev \
        r-cran-rjava
COPY ./dockerImgSetup.R /etc/dockerImgSetup.R
RUN Rscript /etc/dockerImgSetup.R
CMD ["R"]
