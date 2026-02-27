mod_alternative_rating_ui <- function(id) {
  ns <- NS(id)
  
  # Alternative Rating
  nav_panel(
    "Alternative Rating",
    value = id,
    card(
      DTOutput(ns("alternative_rating_table")),
      card_footer(
        style = "display: flex; justify-content: space-between; align-items: center;",
        actionButton(ns("save_rating"), "Save Rating", icon = icon("save"), class="btn-primary"),
        actionButton(ns("import_rating"), "Import Rating", icon = icon("upload"))
      )
    )
  )
}

mod_alternative_rating_server <- function(id, user_coc) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    db_has_changed <- reactiveVal(NA)
    
    ratable_projects <- reactiveVal(NULL)
    rv_uploaded <- reactiveVal(NULL)
    
    observe({
      req(user_coc$coc_version_id)
      req(is.na(db_has_changed()) || db_has_changed())

      ratable_projects(
        get_db_query(
          "SELECT 
            p.project_id, 
            p.organization_name, 
            p.project_name, 
            p.grant_number, 
            p.funding_action, 
            p.project_type, 
            p.target_population, 
            pe.met_hud_thresholds,
            pe.met_coc_thresholds,
            pe.weighted_score,
            pe.date_updated
          FROM projects p
          LEFT JOIN project_evaluations pe ON p.project_id = pe.project_id
          LEFT JOIN lookups l ON p.funding_action = l.reference_id
          WHERE p.coc_version_id = $1 AND p.funding_action IS NOT NULL AND l.value <> 'Ignore'",
          params = list(user_coc$coc_version_id)
        ) |>
          fmutate(
            met_hud_thresholds = factor_yesno(met_hud_thresholds),
            met_coc_thresholds = factor_yesno(met_coc_thresholds)
          )
      )
    }) # end observe that updates ratable_projects
    
    # Alternative Rating table
    output$alternative_rating_table <- renderDT({
      
      data <- ratable_projects()
      
      shiny::validate(need(
        nrow(data) > 0, 
        "No projects to rate"
      ))
      
      editable_cols <- c("met_hud_thresholds", "met_coc_thresholds", "weighted_score")
      
      ## filter out Ignores by default-----
      initial_filter <- vector("list", ncol(data))
      initial_filter[[which(names(data) == "funding_action")]] <- list(search = '["Renew","Reallocate","Replace","New","Expand"]')
      
      colnames <- unname(project_variable_labels[names(data)])
      
      met_hud_input_id <- ns("set_met_hud_thresholds")
      met_coc_input_id <- ns("set_met_coc_thresholds")
      
      header_cb <- glue::glue("
        var thead = $(this.api().table().header());
        thead.find('th').each(function() {{
          var colName = $(this).text().trim();
          debugger;
          if (colName === 'Met HUD Thresholds') {{
            $(this).html(
              'MET HUD THRESHOLDS<div style=\"margin-top:4px;\">' +
              '<button class=\"btn btn-xs btn-success\" style=\"margin-right:2px;\" ' +
                'onclick=\"Shiny.setInputValue(\\'{met_hud_input_id}\\', \\'Yes\\', {{priority: \\'event\\'}})\">✓ All</button>' +
              '<button class=\"btn btn-xs btn-danger\" ' +
                'onclick=\"Shiny.setInputValue(\\'{met_hud_input_id}\\', \\'No\\', {{priority: \\'event\\'}})\">✗ None</button>' +
              '</div>'
            );
          }}
          
          if (colName === 'Met CoC Thresholds') {{
            $(this).html(
              'MET COC THRESHOLDS<div style=\"margin-top:4px;\">' +
              '<button class=\"btn btn-xs btn-success\" style=\"margin-right:2px;\" ' +
                'onclick=\"Shiny.setInputValue(\\'{met_coc_input_id}\\', \\'Yes\\', {{priority: \\'event\\'}})\">✓ All</button>' +
              '<button class=\"btn btn-xs btn-danger\" ' +
                'onclick=\"Shiny.setInputValue(\\'{met_coc_input_id}\\', \\'No\\', {{priority: \\'event\\'}})\">✗ None</button>' +
              '</div>'
            );
          }}
        }});
      ")
            
            
      initialize_inline_edit_table_ui(
        data,
        tableID = "alternative_rating_table",
        initial_filter = initial_filter,
        column_defs = list(
          list(
            targets =c(which(names(data) %in% c("funding_action", "date_updated")) - 1),
            className = "hidden",
            visible = FALSE
          )
        ),
        formatting = list(
          function(x) formatStyle(
            x,
            columns = c("organization_name", "project_name"),
            `white-space` = "nowrap",
            `overflow` = "hidden",
            `max-width` = "400px"
          ),
          function(x) formatStyle(
            x,
            columns = editable_cols,
            backgroundColor = USER_ENTRY_BG_COLOR
          )
        ),
        colnames = colnames,
        cols_to_disable = setdiff(names(data), editable_cols),
        header_cb = header_cb
      )
    })
    
    # Update alternative rating data when cell is edited
    observeEvent(input$alternative_rating_table_cell_edit, {
      info <- input$alternative_rating_table_cell_edit
      
      current_data <- ratable_projects()
      
      current_data[info$row, info$col + 1] <- info$value
      
      ratable_projects(current_data)
    }, ignoreInit = TRUE) # end alt rating table cell edit
    
    ## datatable proxy-----
    # By updating a proxy (via `replaceData`), updates are faster and don't "flicker" the table
    # However it doesn't work when adding new rows
    projects_table_proxy <- dataTableProxy(ns("alternative_rating_table"))
    
    observe({
      req(ratable_projects())
      replaceData(projects_table_proxy, ratable_projects(), resetPaging = FALSE)
    })
    
    # Handle yes-to-all feature for Met HUD/CoC Threshold columns
    observeEvent(input$set_met_hud_thresholds, {
      req(input$set_met_hud_thresholds)
      
      ratable_projects(
        copy(ratable_projects())[, met_hud_thresholds := input$set_met_hud_thresholds]
      )
    })
    
    observeEvent(input$set_met_coc_thresholds, {
      req(input$set_met_coc_thresholds)
      
      ratable_projects(
        copy(ratable_projects())[, met_coc_thresholds := input$set_met_coc_thresholds]
      )
    })
    
    
    # Save ----------------------
    observeEvent(input$save_rating, {
      req(ratable_projects())
      params_list <- ratable_projects() |>
        fmutate(created_by = user_coc$username, new_date_updated = get_db_timestamp()) |>
        fselect(project_id, met_hud_thresholds, met_coc_thresholds, created_by, new_date_updated, date_updated) |>
        as.list() |>
        unname()
      
      rows_changed <- db_execute("
        INSERT INTO project_evaluations (project_id, method, met_hud_thresholds, met_coc_thresholds, created_by)
        VALUES ($1, 'outside', $2, $3, $4)
        ON CONFLICT (project_id) DO UPDATE SET
          method = EXCLUDED.method,
          met_hud_thresholds = EXCLUDED.met_hud_thresholds,
          met_coc_thresholds = EXCLUDED.met_coc_thresholds,
          date_updated = $5,
          updated_by   = EXCLUDED.created_by
        WHERE date_updated = $6",
        params = params_list
      )
      
      if(rows_changed == 0) {
        showNotification("Someone recently edited this data! Refreshing your view. Resubmit when you're ready.", type = "message")
        db_has_changed(TRUE)
      } else if(rows_changed < fnrow(ratable_projects())) {
        showNotification("Someone recently edited one or more project ratings! Refreshing your view. Resubmit when you're ready.", type = "message")
        db_has_changed(TRUE)
      } else {
        db_has_changed(NA)
        showNotification("Saved rating info!", type = "message")
      }
    }) # end save_rating
    
    
    # Importing --------------------
    observeEvent(input$import_rating, {
      
      # Step 1: Show upload modal
      showModal(
        modalDialog(
          title = "Import Outside Ratings",
          size = "l",
          easyClose = FALSE,
          
          # Step containers
          div(
            id = ns("step_upload"),
            fileInput(
              ns("rating_file"),
              "Upload File (CSV or Excel)",
              accept = c(".csv", ".xlsx")
            ),
          ),
          
          hidden(
            div(
              id = ns("step_mapping"),
              uiOutput(ns("field_mapping_ui"))
            )
          ),
          
          # Footer buttons (always visible, controlled by JS)
          footer = tagList(
            actionButton(ns("import_back"), "Back"),
            actionButton(ns("import_next"), "Next"),
            modalButton("Cancel")
          )
        )
      )
    }) # end import_rating
    
    # Track current step
    current_step <- reactiveVal(1)
    
    # handle import pop-up button visibility and labeling
    observe({
      req(user_coc$coc_version_id)
      req(input$import_rating)
      step <- current_step()
      
      if (step == 1) {
        shinyjs::show("step_upload")
        shinyjs::hide("step_mapping")
        shinyjs::hide("import_back")
        shinyjs::show("import_next")
        updateActionButton(session, ns("import_next"), label = "Next")
        
      } else if (step == 2) {
        shinyjs::hide("step_upload")
        shinyjs::show("step_mapping")
        shinyjs::show("import_back")
        updateActionButton(session, ns("import_next"), label = "Submit")
      }
    })
    
    # Next/Submit button
    observeEvent(input$import_next, {
      step <- current_step()
      
      if (step == 1) {
        req(input$rating_file)
        # Validate file type
        ext <- tools::file_ext(input$rating_file$datapath)
        
        shiny::validate(
          need(ext %in% c("csv", "xlsx"),
               "File must be a .csv or .xlsx")
        )
        
        # Read file
        uploaded <- tryCatch({
          if (ext == "csv") {
            data.table::fread(input$rating_file$datapath)
          } else {
            readxl::read_xlsx(input$rating_file$datapath)
          }
        }, error = function(e) NULL)
        
        shiny::validate(
          need(!is.null(uploaded), "Unable to read file.")
        )
        
        rv_uploaded(uploaded)
        
        # Move to mapping step
        current_step(2)
        
      } else if (step == 2) {
        # Submit logic
        req(rv_uploaded())
        # perform mapping + validation + update ratable_projects
        mapping <- sapply(names(ratable_projects), function(f) input[[paste0("map_", f)]])
        
        # Remove empty mappings
        mapping <- mapping[mapping != ""]
        
        imported <- uploaded[, mapping, drop = FALSE]
        colnames(imported) <- names(mapping)
        
        # --------------------
        # VALIDATION
        # --------------------
        
        # 1️⃣ Must have project identifier
        validate(
          need(
            "project_id" %in% names(imported) ||
              all(c("organization_name", "project_name") %in% names(imported)),
            "File must contain either project_id OR organization_name + project_name."
          )
        )
        
        # Pull valid projects
        valid_projects <- get_db_query("SELECT project_id, organization_name, project_name FROM projects")
        
        if ("project_id" %in% names(imported)) {
          validate(
            need(
              all(imported$project_id %in% valid_projects$project_id),
              "Some project_id values not found in database."
            )
          )
        } else {
          merged <- dplyr::left_join(
            imported,
            valid_projects,
            by = c("organization_name", "project_name")
          )
          validate(
            need(!any(is.na(merged$project_id)),
                 "Some organization_name + project_name combinations not found.")
          )
          imported$project_id <- merged$project_id
        }
        
        # 2️⃣ Validate lookups
        validate_lookup <- function(col, ref_type) {
          if (col %in% names(imported)) {
            valid_vals <- LOOKUPS[
              reference_type == ref_type
            ]$value
            
            validate(
              need(all(imported[[col]] %in% valid_vals),
                   paste("Invalid values found in", col))
            )
          }
        }
        
        validate_lookup("funding_action", "funding_action")
        validate_lookup("target_population", "target_population")
        validate_lookup("project_type", "project_type")
        
        # 3️⃣ Boolean threshold conversion
        normalize_bool <- function(x) {
          x <- tolower(as.character(x))
          
          yes_vals <- c("1", "yes", "true", "y", "t")
          no_vals  <- c("0", "no", "false", "n", "f")
          
          ifelse(x %in% yes_vals, 1,
                 ifelse(x %in% no_vals, 0, NA))
        }
        
        if ("met_hud_thresholds" %in% names(imported)) {
          imported$met_hud_thresholds <- normalize_bool(imported$met_hud_thresholds)
          validate(need(!any(is.na(imported$met_hud_thresholds)),
                        "Invalid boolean values in met_hud_thresholds"))
          imported$met_hud_thresholds <- factor_yesno(imported$met_hud_thresholds)
        }
        
        if ("met_coc_thresholds" %in% names(imported)) {
          imported$met_coc_thresholds <- normalize_bool(imported$met_coc_thresholds)
          validate(need(!any(is.na(imported$met_coc_thresholds)),
                        "Invalid boolean values in met_coc_thresholds"))
          imported$met_coc_thresholds <- factor_yesno(imported$met_coc_thresholds)
        }
        
        # -------------
        # UPDATE REACTIVE
        # -------------
        
        current <- ratable_projects()
        updated <- current |>
          join(
            imported, 
            on ="project_id", 
            suffix = c("", "_import")
          )
        
        ratable_projects(updated)
        
        removeModal()
        showNotification("Import successful!", type = "message")
      }
    })
    
    # Back button
    observeEvent(input$import_back, {
      step <- current_step()
      if (step > 1) current_step(step - 1)
    })
    
    output$field_mapping_ui <- renderUI({
      
      uploaded <- rv_uploaded()
      upload_cols <- colnames(uploaded)
      fields <- names(ratable_projects())
      
      
      tagList(
        h4("Map Fields"),
        layout_columns(
          strong("Fields in ORR"),
          strong("Fields in Upload"),
          col_widths = c(6, 6)
        ),
        
        lapply(names(ratable_projects()), function(field) {
          layout_columns(
            div(field),
            selectInput(
              ns(paste0("map_", field)),
              label = NULL,
              choices = c("Choose one" = "", colnames(rv_uploaded())),
              selected = ""
            ),
            col_widths = c(6, 6)
          )
        })
      )
    }) # end field_mapping_ui
    
  }) # end module server
}
