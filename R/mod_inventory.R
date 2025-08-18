mod_inventory_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    "Review Projects",
    value = id,
    card(
      card_body(
        fillable = FALSE,
        DTOutput(ns("projects_table"))
      ),
      card_footer(
        actionButton(ns("add_project_btn"), "Add New Project"),
        actionButton(ns("view_giw_btn"), "View GIW Data")
      )
    )
  )
}

mod_inventory_server <- function(id, user_coc) {
  moduleServer(id, function(input, output, session) {
    user_columns <- c("dv_renewal", "grant_number", "coc_amount_awarded_last_year", "coc_amount_expended_last_year", "coc_funding_requested", "funding_action")
    
    refresh_trigger <- reactiveVal(0)
    
    project_data <- reactive({
      req(user_coc$coc_instance_id)
      refresh_trigger()
      
      data <- get_db_query(
        "SELECT * FROM projects WHERE coc_instance_id = $1", 
        params = user_coc$coc_instance_id
      ) %>% 
        fselect(-coc_instance_id, -date_created, -date_updated, -created_by, -updated_by) %>%
        fmutate(
          funding_action = convert_to_factor(., "funding_action"),
          project_type = convert_to_factor(., "project_type"),
          target_population = convert_to_factor(., "target_population"),
          dv_renewal = factor_yesno(dv_renewal),
          mckinneyvento = factor_yesno(mckinneyvento),
          mckinneyventoyhdp = factor_yesno(mckinneyventoyhdp),
          is_dedicated_ch_fam = factor_yesno(is_dedicated_ch_fam),
          is_dedicated_ch_ind = factor_yesno(is_dedicated_ch_ind),
          is_dedicated_dv = factor_yesno(is_dedicated_dv)
        )
    })
    
    # Projects table -----
    output$projects_table <- renderDT({
      req(project_data())
      data <- project_data()
      
      validate(need(
        nrow(data) > 0, 
        "No rows"
      ))
      
      # filter out Ignores by default
      initial_filter <- vector("list", ncol(data))
      initial_filter[[which(names(data) == "funding_action")]] <- list(search = '["Renew","Reallocate","Replace","New","Expand"]')
      
      initialize_table_ui(data, user_columns, session$ns("projects_table"), initial_filter)
    })
    
    # inline edit handling ------
    validity_pre_checks <- function(project_data, fundingSource, val) {
      if(val == "Reallocate") {
        if(fundingSource == "DV" && project_data$project_type == "SSO - CE") {
          showNotification(
            "According to the FY2024 NOFO, you cannot reallocate a DV SSO-CE 
            Renewal project. Please select a different Funding Action."
          )
          return(FALSE)
        }
      } else if(val == "Replace") {
        if(project_data$mckinneyventoyhdp != "Yes") {
          showNotification(
            "It looks like you are trying to replace a non-YHDP project. Only 
            YHDP projects can be replaced. If this is not a YHDP project, 
            please mark the McKinney- Vento: YHDP field as 'No'"
          )
          return(FALSE)
        }
      }
      return(TRUE)
    }

    update_db_and_cell <- function(info, col_name, project_row_data, new_value) {
      # Update database
      project_id <- ifelse(
        is.na(info$project_id) || is.null(info$project_id),
        as.character(project_row_data$project_id),
        info$project_id
      )

      DBI::dbExecute(
        DB_CON,
        sprintf(
          "UPDATE projects SET %s = $1 WHERE project_id = $2",
          DBI::dbQuoteIdentifier(DB_CON, col_name)
        ), 
        params = list(as.character(new_value), project_id)
      )

      # Update cell
      # We send info$value, which is the user-friendly text ("Reallocate", "Yes", etc.)
      shinyjs::runjs(
        sprintf("
          var table_id = '%s';
          var row = %s;
          var col = %s;
          var data = '%s';
          
          var table = $('#' + table_id + ' table').DataTable();
          debugger;
          table.cell(row - 1, col).data(data).draw();
        ", 
                session$ns("projects_table"), 
                info$row, 
                info$col, 
                info$value
        ))
      
      
      message(sprintf("Updated project_id=%s, column=%s to '%s'",
                      project_id, col_name, new_value))
      
    }
    
    observeEvent(input$projects_table_cell_edit, {
      req(project_data())
      
      info <- input$projects_table_cell_edit

      col_name <- colnames(project_data())[info$col + 1]

      project_row_data <- project_data()[info$row]
      
      fundingSource <- ifelse(
        project_row_data$mckinneyventoyhdp == "Yes",
        "YHDP",
        ifelse(
          isTruthy(project_row_data$dv_renewal == "Yes" || project_row_data$target_population == "DV"),
          "DV",
          "CoC"
        )
      )
      
      new_value <- ifelse(
        is.factor(project_data()[[col_name]]),
        ifelse(
          identical(levels(project_data()[[col_name]]), c("Yes","No")),
          ifelse(info$value == "Yes", 1, 0),
          lookups[reference_type == col_name & value == info$value]$reference_id
        ),
        info$value
      )
      
      is_valid <- TRUE
      if(col_name == "funding_action" && info$value %in% c("Reallocate","Replace")) {
        is_valid <- validity_pre_checks(project_row_data, fundingSource, info$value)
      }

      req(is_valid && !identical(info$value, info$oldValue))

      # Handle Reallocation and Replace
      modal_submission <- reactiveVal(NULL)
      observeEvent(modal_submission(), {
        req(modal_submission())
        update_db_and_cell(info, col_name, project_row_data, new_value)
      }, ignoreNULL = TRUE, once=TRUE)

      if(info$value %in% c("Reallocate", "Replace")) {
        form_type = ifelse(
          info$value == "Replace", 
          "YHDP Replacement", 
          paste0(fundingSource, " Reallocation")
        )
        
        showModal(
          mod_inventory_add_project_ui(
            session$ns("add_project"), 
            form_type = form_type,
            project_to_replace = ifelse(info$value == "Replace",project_data, NULL)
          )
        )
        modal_submission <- mod_inventory_add_project_server(
          "add_project", 
          form_type = form_type,
          funding_source = fundingSource,
          user_coc = user_coc,
          parent_session = session
        )
      } 
      # Handle others
      else {
        update_db_and_cell(info, col_name, project_row_data, new_value)
      }
    }, ignoreInit = TRUE)
    
    # Add additonal project handling ----
    observeEvent(input$add_project_btn, {
      showModal(
        mod_inventory_add_project_ui(session$ns("add_project"))
      )
      mod_inventory_add_project_server("add_project", user_coc = user_coc)
    })
    
    # View GIW Data -------
    giw_data <- reactive({
      get_db_tbl("giw")[coc == user_coc$coc]
    })
    
    observeEvent(input$view_giw_btn, {
      showModal(
        modalDialog(
          title = "GIW",
          DT::renderDT(
            giw_data(),
            fillContainer = TRUE,
            options = list(
              pageLength = 200,
              scrollY = "400px"
            )
          ),
          size="xl",
          easyClose = TRUE
        )
      )
    })
    
  }) # end moduleServer
}

