mod_ranking_widget_ui <- function(bucket_name) {
  ns <- NS(bucket_name)
  
  textOutput(ns("title"))
  uiOutput(ns("bucket"))
}

mod_ranking_widget_server <- function(bucket_name, bucket_projects, user_coc, title) {
  moduleServer(bucket_name, function(input, output, session) {
    ns <- session$ns
    
    output$title <- renderText({
      req(user_coc$coc)
      title
    })
    
    bucket_val <- reactive({
      req(user_coc$coc)
      fcoalesce(fsum(bucket_projects()$coc_funding_recommendation), 0)
    })
    
    
    output$bucket <- renderUI({
      req(user_coc$coc)
      progressBar(
        ns("bucket"),
        value = bucket_val(),
        total = HUD_ARD_REPORT[coc == user_coc$coc][[bucket_name]],
        title = bucket_name,
        display_pct = TRUE,
        format_display = function(value) {
          scales::dollar(value)
        },
        unit_mark = "$"
      )
    })
  })
}