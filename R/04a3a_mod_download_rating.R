mod_download_rating_ui <- function(id) {
  ns <- NS(id)
  
  card_header(
    class = "d-flex justify-content-between align-items-center",
    "Project Rating",
    div(
      class = "d-flex align-items-center",
      # This will show the spinner or the download link when ready
      uiOutput(ns("download_status"), inline = TRUE),
      div(
        class = "dropdown ms-2",
        tags$button(
          id = ns("generate_report_btn"),
          class = "btn btn-primary dropdown-toggle btn-sm",
          type = "button",
          `data-bs-toggle` = "dropdown",
          icon("file"), " Generate Report Card"
        ),
        tags$ul(
          class = "dropdown-menu dropdown-menu-end",
          tags$li(actionLink(ns("dl_current"), "Current Project (PDF)", class = "dropdown-item")),
          tags$li(actionLink(ns("dl_blank"), "Blank Template (PDF)", class = "dropdown-item")),
          tags$hr(class="dropdown-divider"),
          tags$li(actionLink(ns("dl_all"), "All Projects Summary (ZIP)", class = "dropdown-item"))
        )
      )
    )
  )
}

mod_download_rating_server <- function(id, user_coc, selected_project, funding_action, factors_and_scores_for_project) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Stores the final background file path and desired filename
    dl_state <- reactiveVal("idle")
    
    ready_file <- reactiveValues(path = NULL, filename = NULL)
    funding_action_id <- get_lookup_refid(funding_action, "funding_action")
    
    all_factors_and_scores <- reactive({
      get_all_rating_factors_and_scores(user_coc$coc_version_id, funding_action_id)
    })
    
    observeEvent(c(selected_project(), all_factors_and_scores()), {
      shinyjs::toggle(id = "dl_current", condition = fnrow(selected_project()) > 0)
      shinyjs::toggle(id = "dl_all", condition = fnrow(all_factors_and_scores) > 0)
    }, ignoreInit = TRUE)
    
    # ----------------------------------------------------
    # THE BACKGROUND TASK
    # ----------------------------------------------------
    report_task <- shiny::ExtendedTask$new(function(payload) {
      # mirai() must be converted to a promise for ExtendedTask to track it
      promises::as.promise(
        mirai::mirai({
          # This code runs in the background worker
          tmp_dir <- tempdir()
          
          if (payload$type == "single") {
            out_file <- tempfile(fileext = ".pdf", tmpdir = tmp_dir)
            
            build_report(
              data = payload$data,
              project_name = payload$project_name,
              total = payload$total,
              max_pts = payload$max_pts,
              file = out_file
            )
            return(out_file)
            
          } else if (payload$type == "zip") {
            projects <- payload$data |> split(by = "project_id")
            names(projects) <- sapply(projects, function(x) x$project_name[1])
            
            files_to_zip <- character()
            
            for (i in seq_along(projects)) {
              df <- projects[[i]]
              proj_name <- names(projects)[i]
              safe_name <- gsub("[^A-Za-z0-9]", "_", proj_name) 
              out_file <- file.path(tmp_dir, paste0(safe_name, ".pdf"))
              
              build_report(
                data = df,
                project_name = proj_name,
                total = fsum(df$rating_score),
                max_pts = fsum(df$max_point_value),
                file = out_file
              )
              files_to_zip <- c(files_to_zip, out_file)
            }
            
            zip_path <- tempfile(fileext = ".zip", tmpdir = tmp_dir)
            zip::zipr(zipfile = zip_path, files = files_to_zip)
            browser()
            return(zip_path)
          }
        }, payload = payload)
      )
    })
    
    # ----------------------------------------------------
    # DYNAMIC STATUS UI (Inline in the Header)
    # ----------------------------------------------------
    output$download_status <- renderUI({
      state <- dl_state()
      
      if (state == "busy") {
        # Show a small spinner next to the button
        span(class = "text-muted small", style = "color: white !important;", icon("spinner", class = "fa-spin"), " Generating...")
      } else if (state == "ready") {
        # Show a green download link
        downloadLink(
          ns("dl_final"), 
          span(icon("download"), style = "color: white !important", " Download"), 
          class = "btn btn-success btn-sm"
        )
      } else {
        NULL # Idle state
      }
    })
    
    # ----------------------------------------------------
    # TRIGGERS (Invokes background task)
    # ----------------------------------------------------
    get_project_df <- function() {
      factors_and_scores_for_project() |>
        fselect(
          factor_group,
          factor_subgroup,
          piping_text,
          goal,
          performance,
          rating_score,
          max_point_value
        )
    }
    observeEvent(input$dl_current, {
      dl_state("busy")
      # showNotification("PDF generation started in the background...", type = "message")
      
      df <- get_project_df()
      
      payload <- list(
        type = "single",
        filename = paste0(selected_project()$project_name, ".pdf"),
        data = df,
        project_name = selected_project()$project_name,
        total = fsum(df$rating_score),
        max_pts = fsum(df$max_point_value)
      )
      
      ready_file$filename <- payload$filename
      
      later::later(function() {
        report_task$invoke(payload)
      }, delay = 0)
      
    })
    
    iv <- shinyvalidate::InputValidator$new()
    iv$add_rule("project_type_filter", sv_required())
    iv$add_rule("target_population_filter", sv_required())
    observeEvent(input$dl_blank, {
      showModal(
        modalDialog(
          title = 'Blank Report Card Specifications',
          project_type_dropdown <- selectInput(
            inputId = ns("project_type_filter"),
            label = "Select project type",
            choices = get_labelled_lookups("project_type")[MAIN_PROJECT_TYPES]
          ),
          
         if(funding_action == "Renew") 
           target_pop_dropdown <- selectInput(
            inputId = ns("target_population_filter"),
            label = "Select special populations",
            choices = get_labelled_lookups("target_population")[c("DV", "General")]
          )
         else
           NULL,
          footer = tagList(
            actionButton(ns('blank_download'), label='Confirm', class='btn-primary'),
            modalButton(label='Cancel')
          )
        )
      )
    }, ignoreInit = TRUE)
    
    observeEvent(input$blank_download, {
      iv$enable()
      req(iv$is_valid())
      
      dl_state("busy")
      
      removeModal()
      
      df <- get_rating_factors_by_pop_target_type(
        user_coc$coc_version_id, 
        funding_action_id, 
        input$project_type_filter, 
        input$target_population_filter
      )
      
      payload <- list(
        type = "single",
        filename = "Blank_Template.pdf",
        data = df,
        project_name = "Blank Template",
        total = 0, max_pts = 0
      )
      
      ready_file$filename <- payload$filename
      
      later::later(function() {
        report_task$invoke(payload)
      }, delay = 0)
      
    })
    
    observeEvent(input$dl_all, {
      if(fnrow(all_factors_and_scores) == 0)
        showNotification("No projects have scores!", type = "error")
      req(FALSE)
      
      dl_state("busy")
      
      payload <- list(
        type = "zip",
        filename = "All_Reports.zip",
        data = all_factors_and_scores
      )
      
      ready_file$filename <- payload$filename
      
      later::later(function() {
        report_task$invoke(payload)
      }, delay = 0)
      
    })
    
    # ----------------------------------------------------
    # HANDLE TASK COMPLETION
    # ----------------------------------------------------
    
    # Watch for Success
    observeEvent(report_task$result(), {
      ready_file$path <- report_task$result()
      dl_state("ready")
    })
    
    # ----------------------------------------------------
    # ACTUAL DOWNLOAD HANDLER
    # ----------------------------------------------------
    output$dl_final <- downloadHandler(
      filename = ready_file$filename,
      content = function(file) {
        dl_state("idle")
        
        # Instant copy since the file is already generated
        file.copy(ready_file$path, file)
      }
    )
    
  })
}