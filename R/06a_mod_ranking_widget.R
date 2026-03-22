mod_ranking_widget_ui <- function(bucket_name) {
  ns <- NS(bucket_name)
  
  # textOutput(ns("title"))
  uiOutput(ns("bucket"))
}

mod_ranking_widget_server <- function(bucket_name, allocated, total, user_coc, title, rv) {
  moduleServer(bucket_name, function(input, output, session) {
    ns <- session$ns
  
    output$bucket <- renderUI({
      req(user_coc$coc)
      
      progressBar(
        ns("bucket"),
        value = allocated(),
        total = total,
        title = title,
        display_pct = TRUE,
        format_display = function(value) {
          scales::dollar(value)
        },
        status = if(allocated() > total) "danger" else "primary"
      )
    })
  })
}