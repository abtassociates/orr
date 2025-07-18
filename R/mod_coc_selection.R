mod_coc_selection_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    "Select CoC",
    value = id,
    card(
      card_header("Select your Continuum of Care"),
      card_body(
        fillable = FALSE,
        DTOutput(ns('coc_instances_dt')),
        actionButton(ns('edit_coc_instance'),"Edit selected CoC", icon = icon('edit'))
      )
    )
  )
}

mod_coc_selection_server <- function(id, nav_control, projects_data, selected_coc) {
  moduleServer(id, function(input, output, session) {
    
    ## add placeholder reactive DF until coc_instances table is filled in
    coc_instances <- reactive({
      data.frame(coc_instance_id = rep(1,3), 
                 coc = c('TX-600','TX-500','TX-700'), 
                 coc_status = c('Not Started','Not Started','In Progress'),
                 date_updated = c("2025-07-16 13:53:11 EDT","2025-07-16 13:43:11 EDT","2025-07-16 13:33:11 EDT"), 
                 updated_by = c('user1','user1','user1'))
    })
    
    
    ## disable Edit button unless row is selected
    observe({
      if(length(input$coc_instances_dt_rows_selected)==0){
        shinyjs::disable(id = 'edit_coc_instance')
      } else {
        shinyjs::enable(id = 'edit_coc_instance')
      }
    })
    
    output$coc_instances_dt <- renderDT({
      
      datatable(coc_instances(), 
                options = list(dom = 'tpi'),
                editable = FALSE,
                style = 'default',
                filter = list(position = 'top', plain = TRUE),
                selection = 'single'
      )
    })
    
    observeEvent(input$edit_coc_instance, {
      #if (input$coc_select != "") {
        selected_coc(input$coc_select)
        selected_coc(coc_instances()[input$coc_instances_dt_rows_selected,'coc'])
        print(selected_coc())
        
        nav_control("inventory")
        
        # Initialize projects data
        filtered_data <- hic_data |>
          fsubset(CoC_Code == selected_coc()) |>
          fmutate(
            DV_Renewal = NA_character_,
            Grant_Number = NA_character_,
            CoC_Funding_Requested = NA_real_,
            Funding_Action = fifelse(McKinney_Vento == "No", "Ignore", "Renew")
          )
        
        projects_data(filtered_data)
      #}
    })
  })
}