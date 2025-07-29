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
source(here("R/utils/utils.R"))

in_dev_mode <- grepl("ad.abt.local", Sys.info()[["nodename"]]) & !isTRUE(getOption("shiny.testmode"))


source(here("R/utils/get_db_data.R"))
DB_CON <- get_db_connection()

lookups <- get_db_tbl("lookups")
users <- get_db_tbl("users")
cocs <- get_db_tbl("cocs")
coc_instance_users <- get_db_query(
  "SELECT u.*, i.coc 
  FROM coc_instance_users u 
  LEFT JOIN coc_instances i 
  ON u.coc_instance_id = i.coc_instance_id"
)

set.seed(123)
