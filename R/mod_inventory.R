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
    ns <- NS(id)
    user_columns <- c("dv_renewal", "grant_number", "coc_amount_awarded_last_year", "coc_amount_expended_last_year", "coc_funding_requested", "funding_action")
    
    projects_data <- reactiveVal(NULL)
    new_project <- reactiveVal(FALSE)
    
    observe({
      req(user_coc$coc_instance_id)

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
      
      projects_data(data)
    })
    
    # Projects table -----
    output$projects_table <- renderDT({
      if(new_project()) {
        data <- projects_data()
        new_project(FALSE)
      } else {
        data <- isolate(projects_data())
      }

      validate(need(
        nrow(data) > 0, 
        "No rows"
      ))
      
      # filter out Ignores by default
      initial_filter <- vector("list", ncol(data))
      initial_filter[[which(names(data) == "funding_action")]] <- list(search = '["Renew","Reallocate","Replace","New","Expand"]')

      initialize_table_ui(data, user_columns, ns("projects_table"), initial_filter)
    })
    

    projects_table_proxy <- dataTableProxy(ns("projects_table"))

    observe({
      req(projects_data())

      # replaceData is the proxy equivalent of re-rendering. It's much faster.
      replaceData(projects_table_proxy, projects_data())
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
            YHDP projects can be replaced. If this is a YHDP project, 
            please mark the McKinney- Vento: YHDP field as 'Yes'"
          )
          return(FALSE)
        }
      }
      return(TRUE)
    }

    # Update projects -----
    update_db <- function(new_value, col_name, proj_id) {
      DBI::dbExecute(
        DB_CON,
        sprintf(
          "UPDATE projects SET %s = $1 WHERE project_id = $2",
          DBI::dbQuoteIdentifier(DB_CON, col_name)
        ), 
        params = list(as.character(new_value), proj_id)
      )
      message(sprintf("Updated db: project_id=%s, column=%s to '%s'",
                      proj_id, col_name, new_value))
      
    }
    update_datatable <- function(proj_id, col_name, value) {
      # update the reactiveVal that updates the proxy
      # We send info$value, which is the user-friendly text ("Reallocate", "Yes", etc.)
      updated_data <- copy(projects_data())[
        project_id == proj_id, 
        (col_name) := value
      ]
      projects_data(updated_data)
    }
    
    inventory_update <- function(info, value) {
      project_data <- projects_data()[project_id == info$project_id]
      proj_id <- as.character(project_data$project_id)
      col_name <- colnames(projects_data())[info$col + 1]

      update_datatable(proj_id, col_name, value)
      update_db(value, col_name, proj_id)
    }
    
    # Add/append new projects -----
    append_to_datatable <- function(new_project_data) {
      dt_data <- new_project_data  %>% 
        fmutate(
          project_id = "temp",
          mckinneyvento = factor_yesno(mckinneyvento),
          mckinneyventoyhdp = factor_yesno(mckinneyventoyhdp),
          dv_renewal = factor_yesno(dv_renewal),
          coc_amount_awarded_last_year = NA,
          coc_amount_expended_last_year = NA,
          coc_funding_requested = NA,
          geocode = "",                      
          amount_other_public_funding = NA,
          amount_private_funding = NA,
          is_dedicated_ch_fam = factor_yesno(is_dedicated_ch_fam),
          is_dedicated_ch_ind = factor_yesno(is_dedicated_ch_ind),
          is_dedicated_dv = factor_yesno(is_dedicated_dv)
        ) 
      
      # When a new row is added, the inventory_add_project module handles the DB updatewe update the projects_data
      new_row <- project_data %>% 
        fselect(names(projects_data())) %>% 
        fmutate(project_id = as.integer(fmax(projects_data()$project_id) + 1))
      
      updated_data <- rbind(new_row, copy(projects_data()))
      
      projects_data(updated_data)
      new_project(TRUE)
    }
    
    append_to_db <- function(new_project_data) {
      db_data <- new_project_data %>%
        fmutate(
          coc_instance_id = user_coc$coc_instance_id,
          funding_action = get_ref_id(funding_action),
          project_type = get_ref_id(project_type),
          target_population = get_ref_id(target_population),
          created_by = user_coc$username,
          date_created = format(lubridate::now(), "%Y-%m-%d %H:%M:%S")
        )
      
      DBI::dbAppendTable(DB_CON, "projects", db_data)
    }
    
    inventory_append <- function(new_project_data) {
      append_to_datatable(new_project_data)
      append_to_db(new_project_data)

      showNotification("Project submitted successfully.", type = "message")
    }
    
    observeEvent(input$projects_table_cell_edit, {
      req(projects_data())
      
      info <- input$projects_table_cell_edit
      req(!identical(info$value, info$oldValue))
      
      col_name <- colnames(projects_data())[info$col + 1]
      
      info$project_id <- ifelse(
        is.na(info$project_id) || is.null(info$project_id),
        as.character(projects_data()[info$row, "project_id"]),
        info$project_id
      )
      
      project_data <- projects_data()[project_id == info$project_id]
      
      fundingSource <- ifelse(
        project_data$mckinneyventoyhdp == "Yes",
        "YHDP",
        ifelse(
          isTruthy(project_data$dv_renewal == "Yes" || project_data$target_population == "DV"),
          "DV",
          "CoC"
        )
      )
      
      is_factor_col <- is.factor(projects_data()[[col_name]])
      new_value <- ifelse(
        is_factor_col,
        ifelse(
          identical(levels(projects_data()[[col_name]]), c("Yes","No")),
          ifelse(info$value == "Yes", 1, 0),
          lookups[reference_type == col_name & value == info$value]$reference_id
        ),
        info$value
      )
      
      is_valid <- TRUE
      if(col_name == "funding_action" && info$value %in% c("Reallocate","Replace")) {
        is_valid <- validity_pre_checks(project_data, fundingSource, info$value)
        
        # If they can't Reallocate or Replace, bring back old value in table cell
        # we're basically just reversing the `setCellData` function triggered in `inline_editable_datatable.R`
        if(!is_valid) {
          shinyjs::runjs(sprintf(
            "
              var table = $('#%s table').DataTable();
              table.cell(%s, %s).data('%s');
            ", 
            ns("projects_table"), info$row - 1, info$col, info$oldValue
          ))
        }
      }
      req(is_valid)
      
      # Handle Reallocation and Replace
      if(info$value %in% c("Reallocate", "Replace")) {
        form_type <- ifelse(
          info$value == "Replace", 
          "YHDP Replacement", 
          paste0(fundingSource, " Reallocation")
        )
        project_to_replace <- if(info$value == "Replace") project_data else NULL
        show_project_modal(form_type, fundingSource, info, new_value, project_to_replace)
      } 
      # Handle others
      else {
        inventory_update(info, new_value)
      }
    }, ignoreInit = TRUE)
    
    # A function to show the modal and set up the server logic
    show_project_modal <- function(form_type = "New", fundingSource = "", info = NULL, new_value = NULL, project_to_replace = NULL) {
      showModal(
        div(
          id ="add-project-modal",
          mod_inventory_add_project_ui(
            ns("add_project"), 
            form_type = form_type,
            project_to_replace = project_to_replace
          )
        )
      )
      modal_submission <- mod_inventory_add_project_server(
        "add_project", 
        form_type = form_type,
        funding_source = fundingSource,
        user_coc = user_coc,
        parent_session = session
      )
      
      observeEvent(modal_submission$status, {
        req(modal_submission$status)

        # if they simply add a new project, append it
        if(form_type == "New") {
          inventory_append(modal_submission$project_data)
        } 
        # If they reallocated or replaced, we need to both update the reallocated/replaced row
        # AND add the new project
        else {
          inventory_update(info, new_value)
          inventory_append(modal_submission$project_data)
        }

        if(modal_submission$status == "add another") {
          show_project_modal(form_type, fundingSource, info, new_value, project_to_replace)
        }
      }, ignoreNULL = TRUE)
    }
    
    # Add additonal project handling ----
    observeEvent(input$add_project_btn, {
      show_project_modal()
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
