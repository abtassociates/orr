library(shiny)
library(bslib)
library(brandr)
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
library(shinyWidgets)

# ENVIRONMENT DETECTION -----------
IN_DEV_MODE <- Sys.getenv("RSTUDIO") == "1" && !isTRUE(getOption("shiny.testmode"))

# CODE OPTIONS --------------
set.seed(123)
set_collapse(na.rm = TRUE, verbose = FALSE, sort = FALSE)

# COLORS AND THEME -----------
# Need to set this or `brandr::assert_brand_yml` will not work correctly because for non-interactive sessions, it takes the main path
options(BRANDR_BRAND_YML = here::here("_brand.yml"))

USER_ENTRY_BG_COLOR <- "#e6ffe6"

orr_bslib_theme <- bs_theme(
  version = 5,
  brand = TRUE,
  "card-cap-bg" = get_brand_color("dark_blue"),
  "card-cap-color" = "white",
  "card-bg" = "white",
  preset = "zephyr"
)

orr_navbar_options <- navbar_options(theme = 'auto', bg = get_brand_color('dark_blue'))

# UTILS AND DB FUNCTIONS --------------
files <- list.files(here("R/utils"), pattern = "\\.R$", full.names = TRUE)
lapply(files, source)

files <- list.files(here("R/app_db_funcs"), pattern = "\\.R$", full.names = TRUE)
lapply(files, source)


# SET UP DB CONNECTION -----------------
set_up_db_connection(USE_SQLITE = FALSE)


# PREP GLOBAL DATA ---------------
source(here("R/global_data_prep.R"))


# shiny::runApp(port = 4000, launch.browser = T)
