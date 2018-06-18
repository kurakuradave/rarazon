##################################################################
###   For installing packages when building the docker image   ###
##################################################################
pkgs <- c( 
           "caret",
           "devtools",
           "igraph",
           "ggplot2",
           "ggraph",
           "lexicon",
           "randomForest",
           "RWeka",
           "shiny",
           "stringr",
           "tidyr",
           "tidytext",
           "tidyverse",
           "tm",           
           "wordcloud"
)

install.packages(pkgs)

### install RSelenium
devtools::install_github("johndharrison/binman")
devtools::install_github("johndharrison/wdman")
devtools::install_github("ropensci/RSelenium")
