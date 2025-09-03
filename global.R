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


IN_DEV_MODE <- F#grepl("ad.abt.local|ANEPRRDSH-04", Sys.info()[["nodename"]]) & !isTRUE(getOption("shiny.testmode"))

set.seed(123)

# Load all utils functions
files <- list.files(here("R/utils"), pattern = "\\.R$", full.names = TRUE)
lapply(files, source)


USER_ENTRY_BG_COLOR <- "#e6ffe6"
