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
library(httr)
library(httr2)
library(jsonlite)

in_dev_mode <- F#grepl("ad.abt.local", Sys.info()[["nodename"]]) & !isTRUE(getOption("shiny.testmode"))

# Load all utils functions
files <- list.files(here("R/utils"), pattern = "\\.R$", full.names = TRUE)
lapply(files, source)

# Pull global datasets from db
#source(here("R/global_data_prep.R"))

set.seed(123)

user_entered_color <- "#e6ffe6"
