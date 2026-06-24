library(shiny)
library(bslib)
library(brandr)
library(brand.yml)
library(DT)
# library(sortable)
library(fontawesome) # may not be needed
library(collapse)
library(data.table)
library(shinyjs)
library(shinydisconnect)
library(stringr)
library(forcats)
library(DBI)
library(RPostgres)
# library(digest)
library(here)
# library(rhandsontable)
library(shinyvalidate)
library(purrr)
library(shinycssloaders)
library(httr)
library(httr2)
library(jsonlite)
library(shinyWidgets)
# downloads
library(gt)
library(pagedown)
# For Rating Report Cards
library(mirai)
library(promises)
library(writexl)
library(openxlsx2)

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

# Uses:
#   Determining old value for reverting invalid inline entry
#     when server=FALSE, we can grab from the not-yet-updated reactive
#     when server=TRUE, we need to determine the actual row  in case user has filtered 
#   Speed
#     supposedly server=FALSE is faster because the browser does the work, but if anything I think it's slower
#   Code formatting
#     server=FALSE doesn't require `replaceData` but it forces us to do more work in js
#       e.g., if user starts editing a dollar-formatted cell and hits escape, we have to update js to add the formatting back in
DT_USES_SERVER <- TRUE

# PREVENT AUTO-CONVERT TO SCIENTIFIC NOTATION ------------
# otherwise, db saves of dollar amounts could fail
options(scipen = 999)

# LOGGING ---------------
filtered_appender <- function(lines) {
  # Regex to drop lines where an input is transitioning from NULL
  lines <- lines[!grepl("Shiny input change detected in .*: NULL -> ", lines)]
  
  # Pass any remaining lines to the standard stderr appender
  if (length(lines) > 0) {
    logger::appender_stderr(lines)
  }
}

logger::log_appender(filtered_appender)
logger::log_threshold(Sys.getenv("LOG_LEVEL", "DEBUG"))

options(shiny.fullstacktrace = IN_DEV_MODE)
options(shiny.sanitize.errors = FALSE)

# UTILS AND DB FUNCTIONS --------------
files <- list.files(here("R/utils"), pattern = "\\.R$", full.names = TRUE)
lapply(files, source)

files <- list.files(here("R/app_db_funcs"), pattern = "\\.R$", full.names = TRUE)
lapply(files, source)


# SET UP DB CONNECTION -----------------
set_up_db_connection()


# PREP GLOBAL DATA ---------------
source(here("R/global_data_prep.R"))

# QUESTION: Does this get triggered on crashes?
shiny::onStop( function(){
  message("Application exited. Closing pool and mirai daemons")
  close_pool()
  
  if (shiny::isRunning() && Sys.getenv("RSTUDIO") == "1") {
    try(tools::pskill(tunnel), silent = TRUE)
  }
  
  daemons(0)
})

# For Rating Report Cards
daemons(2, output = TRUE, sync = TRUE)
mirai::everywhere({
  library(gt)
  library(pagedown)
  library(dplyr)
  library(zip)
  
  source(here("R/utils/build_report.R"))
}, .options = list(seed = TRUE))

# shiny::runApp(port = 4000, launch.browser = T)
