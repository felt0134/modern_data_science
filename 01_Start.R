# build

rm(list=ls())

#load packages
pkgs <- c('rstudioapi','tidyverse','mdsr')
lapply(pkgs, library, character.only = TRUE)

# Set working directory to local directory
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))