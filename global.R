library(shiny)
library(bslib)
library(DT)
library(sortable)
library(fontawesome) # may not be needed
library(collapse)
library(data.table)
library(shinyjs)
library(shinydisconnect)
library(stringr)
library(forcats)
library(DBI)
library(RPostgres)
library(digest)
library(here)
library(rhandsontable)
library(shinyvalidate)
library(purrr)
library(shinycssloaders)


# Load all utils functions
files <- list.files(here("R/utils"), pattern = "\\.R$", full.names = TRUE)
lapply(files, source)

in_dev_mode <- grepl("ad.abt.local", Sys.info()[["nodename"]]) & !isTRUE(getOption("shiny.testmode"))

# Pull global datasets from db
source(here("R/global_data_prep.R"))

set.seed(123)
