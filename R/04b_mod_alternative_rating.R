mod_alternative_rating_ui <- function(id) {
  ns <- NS(id)
  
  # Alternative Rating
  nav_panel(
    "Alternative Rating",
    value = id,
    card(
      card_body(
        fillable = FALSE,
        min_height = "65vh",
        max_height = "71vh",
        DTOutput(ns("alternative_rating_table"))
      ),
      card_footer(
        style = "display: flex; justify-content: space-between; align-items: center;",
        downloadButton(ns("download_template"), "Download Template", icon = icon("download")),
        actionButton(ns("import_rating"), "Import Rating", icon = icon("upload")) #,
        # actionButton(ns("save_rating"), "Save Rating", icon = icon("save"), class="btn-primary")
      )
    )
  )
}

mod_alternative_rating_server <- function(id, user_coc, nav_control) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ratable_projects <- reactiveVal(NULL)
    rv_uploaded <- reactiveVal(NULL)
    refresh_trigger <- reactiveVal(NA)
    rerender_table <- reactiveVal(0)
    
    observeEvent(c(user_coc$coc_version_id, refresh_trigger(), user_coc$projects_updated), {
      req(user_coc$coc_version_id)
      
      ratable_projects(
        get_alternative_rating(
          user_coc$coc_version_id
        ) %>%
          format_table_data()
      )
    }, ignoreInit = TRUE) # end observe that updates ratable_projects
    
    ratable_projects_no_vid <- reactive({
      req(isTruthy(fnrow(ratable_projects()) > 0))
      ratable_projects() |> fselect(-version_id)
    })
    
    # Alternative Rating table
    # 1. Create a helper function to ensure data formatting is identical
    format_table_data <- function(df) {
      
      df %>%
        fmutate(
          met_hud_thresholds = factor_yesno(met_hud_thresholds),
          met_coc_thresholds = factor_yesno(met_coc_thresholds),
          project_type = convert_to_factor(., "project_type", textToNum = F),
          target_population = convert_to_factor(., "target_population", textToNum = F)
        )
    }
    
    output$alternative_rating_table <- renderDT({
      req(user_coc$coc_version_id)
      rerender_table()
      data <- isolate(ratable_projects_no_vid())
      
      shiny::validate(need(
        nrow(data) > 0, 
        "No projects to rate"
      ))
      #data <- format_table_data(data)
        
      editable_cols <- c("met_hud_thresholds", "met_coc_thresholds", "weighted_score")
      
      header_cb <- get_js_script("alternative_dt.js")
      header_cb <- gsub('__MET_HUD_INPUT_ID__', ns("set_met_hud_thresholds"), header_cb)
      header_cb <- gsub('__MET_COC_INPUT_ID__', ns("set_met_coc_thresholds"), header_cb)
      
      initialize_inline_edit_table_ui(
        data,
        column_defs = list(
          list(
            targets = which(names(data) %in% c("funding_action", "date_updated")) - 1,
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
        colnames = unname(variable_labels[names(data)]),
        cols_to_disable = setdiff(names(data), editable_cols),
        header_cb = header_cb,
        options = list(
          autoWidth = FALSE,
          paging = TRUE,
          pageLength = 100,
          dom = 'frtip'
        )
      )
    }, server=FALSE)
    
    alt_rating_update <- function() {
      updated_project_evaluations = get_updated_project_evaluations(user_coc$username, ratable_projects())
      needs_refresh <- update_project_evaluations_db(get_db_pool(), updated_project_evaluations)
      
      if(!needs_refresh) {
        ratable_projects()[
          project_id %in% updated_project_evaluations$project_id,
          version_id := version_id + 1
        ]
        
        status <- calculate_coc_status(user_coc$coc_version_id, current_data$project_id)
        update_coc_status(user_coc, status)
      } else {
        refresh_trigger(refresh_trigger() + 1)
      }
    }
    
    # Update alternative rating data when cell is edited
    observeEvent(input$alternative_rating_table_cell_edit, {
      info <- input$alternative_rating_table_cell_edit
      
      req(!identical(info$value, info$oldValue))
      
      current_data <- copy(ratable_projects())
      
      current_data[info$row, (info$col + 1) := info$value]
      
      ratable_projects(current_data)
      
      alt_rating_update()
    }, ignoreInit = TRUE) # end alt rating table cell edit

    # Handle yes-to-all feature for Met HUD/CoC Threshold columns
    set_all_thresholds_handler <- function(colname) {
      input_name <- paste0("set_", colname)
      observeEvent(input[[input_name]], {
        req(input[[input_name]])
        
        visible_rows <- input$alternative_rating_table_rows_all  # indices of filtered rows
        
        updated <- copy(ratable_projects())
        updated[visible_rows, (colname) := as.integer(input[[input_name]])]
        
        ratable_projects(updated)
        
        alt_rating_update()
      })
    }
    set_all_thresholds_handler("met_hud_thresholds")
    set_all_thresholds_handler("met_coc_thresholds")
    
    # Save ----------------------
    get_updated_project_evaluations <- function(username, ratable_projects) {
      ratable_projects |>
        fmutate(
          created_by = username,
          met_hud_thresholds = met_hud_thresholds == 'Yes',
          met_coc_thresholds = met_coc_thresholds == 'Yes'
        ) |>
        fselect(project_id, met_hud_thresholds, met_coc_thresholds, weighted_score, created_by, version_id)
    }
    
    # observeEvent(input$save_rating, {
    #   req(ratable_projects())
    #   alt_rating_update()
    # }, ignoreInit = TRUE) # end save_rating
    
    # Download Template (to facilitate import)
    output$download_template <- downloadHandler(
      filename = "ORR-Alternative-Rating-Tool-Template.xlsx",
      content = function(file) {
        dt <- ratable_projects_no_vid()
        writexl::write_xlsx(
          dt,
          path = file,
          format_headers = FALSE,
          col_names = TRUE
        )
      }
    )
    
    # Importing --------------------
    observeEvent(input$import_rating, {
      
      # Step 1: Show upload modal
      showModal(
        modalDialog(
          title = "Import Outside Ratings",
          size = "l",
          easyClose = FALSE,
          
          # Step 1: Upload
          div(
            id = ns("step_upload"),
            fileInput(
              ns("rating_file"),
              "Upload File (CSV or Excel)",
              accept = c(".csv", ".xlsx")
            ),
          ),
          
          # Step 2: Preview (NEW)
          hidden(
            div(
              id = ns("step_preview"),
              h5("Data Preview"),
              p("Here are the first 5 rows of your uploaded file. Review to ensure it uploaded correctly."),
              DTOutput(ns("import_preview_table")) |> withSpinner()
            )
          ),
          
          # Step 3: Mapping fields
          hidden(
            div(
              id = ns("step_mapping"),
              uiOutput(ns("field_mapping_ui"))
            )
          ),
          
          hidden(div(id = ns("import_error_box"), class = "text-danger")),
          
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
    
    # Preview Table Render
    output$import_preview_table <- renderDT({
      req(rv_uploaded())
      # req() delays rendering until Step 2 is active, which prevents DT from collapsing inside hidden UI elements
      req(current_step() == 2) 
      
      cols_to_widen <- intersect(
        c("project_name", "organization_name"), 
        colnames(rv_uploaded())
      )
      
      DT::datatable(
        head(rv_uploaded(), 5),
        options = list(
          dom = 't',
          scrollX = TRUE,
          ordering = FALSE,
          autoWidth = FALSE # Important so DT respects our custom widths
        ),
        rownames = FALSE
      ) |> 
        DT::formatStyle(
          columns = cols_to_widen,
          minWidth = "300px" # Adjust width as needed
        )
      
    })
    
    # handle import pop-up button visibility and labeling
    observe({
      req(user_coc$coc_version_id)
      req(input$import_rating)
      step <- current_step()
      
      # Hide all steps by default
      shinyjs::hide("step_upload")
      shinyjs::hide("step_preview")
      shinyjs::hide("step_mapping")
      
      if (step == 1) {
        shinyjs::show("step_upload")
        shinyjs::hide("import_back")
        shinyjs::show("import_next")
        updateActionButton(session, ns("import_next"), label = "Next")
        
      } else if (step == 2) {
        shinyjs::show("step_preview")
        shinyjs::show("import_back")
        updateActionButton(session, ns("import_next"), label = "Submit")
      } else if (step == 3) {
        shinyjs::show("step_mapping")
        shinyjs::show("import_back")
        updateActionButton(session, ns("import_next"), label = "Submit")
      }
    })
    
    show_modal_error <- function(condition, message) {
      if (!isTRUE(condition)) {
        # Inject the text
        shinyjs::html("import_error_box", message)
        # Reveal the error box
        shinyjs::show("import_error_box")
        # Stop execution
        req(FALSE) 
      }
    }
    
    
    # Next/Submit button
    observeEvent(input$import_next, {
      shinyjs::hide("import_error_box") 
      
      step <- current_step()
      
      if (step == 1) {
        # Initial validation
        show_modal_error(!is.null(input$rating_file), "Please upload a file.")
        
        ext <- tools::file_ext(input$rating_file$datapath)
        show_modal_error(tools::file_ext(input$rating_file$datapath) %in% c("csv", "xlsx"), "File must be CSV or XLSX")
        
        # Read file
        uploaded <- tryCatch({
          if (ext == "csv") {
            data.table::fread(input$rating_file$datapath)
          } else {
            readxl::read_xlsx(input$rating_file$datapath)
          }
        }, error = function(e) {
          log_error(paste0("Importing Alt Rating...:", e$message))
          NULL
        })
        
        # Validate file came through
        show_modal_error(!is.null(uploaded), "Unable to read file.")
        
        rv_uploaded(uploaded)
        
        # Move to mapping step
        current_step(2)
        
      } else if(step == 2) {
        current_step(3)
      } else if (step == 3) {
        # perform mapping + validation + update ratable_projects
        mapping <- sapply(
          c("project_id","organization_name", "project_name","met_hud_thresholds","met_coc_thresholds","weighted_score"), 
          function(f) input[[paste0("map_", f)]]
        )
        
        # Remove empty mappings (ones they didn't select in dropdown)
        mapping <- mapping[mapping != ""]
        
        # Rename to expected fields using mapping
        imported <- rv_uploaded() |> rename(!!!mapping)

        # VALIDATION: 1️⃣ Must have project identifier
        show_modal_error(
          "project_id" %in% names(imported) ||
            all(c("organization_name", "project_name") %in% names(imported)), 
          "File must contain either project_id OR organization_name + project_name."
        )
        
        # If "project_id" isn't in upload, bring it in via org name + proj name
        if (!"project_id" %in% names(imported)) {
          imported <- imported |>
            join(
              ratable_projects() |> fselect(project_id, organization_name, project_name),
              on = c("organization_name", "project_name"),
              column = TRUE
            )
        }
        
        # VALIDATION: Uploaded projects not found in db
        show_modal_error(
          all(imported$project_id %in% ratable_projects()$project_id),
          "Some projects in your upload were not found in the list of ratable projects."
        )
        
        # VALIDATION: Ratable projects not in upload
        show_modal_error(
          all(ratable_projects()$project_id %in% imported$project_id),
          "Some ratable projects were not found in your upload. Please make sure you have uploaded all projects you wish to rate."
        )
        
        # VALIDATION: Missing rating cols
        rating_cols <- c("met_hud_thresholds","met_coc_thresholds","weighted_score") 
        rating_cols_present <- rating_cols %in% names(imported)
        
        show_modal_error(
          all(rating_cols_present),
          glue::glue("Your upload file is missing {rating_cols_present[rating_cols]}")
        )
        
        # just keep the columns we need
        imported <- imported |> 
          fselect(project_id, met_hud_thresholds, met_coc_thresholds, weighted_score)
        
        
        # VALIDATION: Invalid boolean vals
        # 3️⃣ Boolean threshold conversion
        normalize_bool <- function(x) {
          x <- tolower(as.character(x))
          
          yes_vals <- c("1", "yes", "true", "y", "t")
          no_vals  <- c("0", "no", "false", "n", "f")
          
          fcase(
            x %in% yes_vals, 1,
            x %in% no_vals, 0, 
            !is.na(x), -9,
            default = NA
          )
        }
        
        for(v in c("met_hud_thresholds", "met_coc_thresholds")) {
          imported[[v]] <- normalize_bool(imported[[v]])
          show_modal_error(
            !anyv(imported[[v]], -9), 
            glue::glue("Invalid boolean values in {v}")
          )
          imported[[v]] <- factor_yesno(imported[[v]])
        }
        
        # VALIDATION: Invalid score
        show_modal_error(
          all(is.na(imported$weighted_score) | imported$weighted_score %between% c(0, 100)),
          "Weighted scores must be an integer between 0 and 100"
        )
        
        # -------------
        # UPDATE REACTIVE
        # -------------
        update_cols <- setdiff(names(imported), "project_id")
        current <- copy(ratable_projects())
        current[
          imported, 
          on = "project_id", 
          (update_cols) := lapply(update_cols, function(col) {
            fcoalesce(get(paste0("i.", col)), DT::coerceValue(get(col), get(paste0("i.", col))))
          })
        ]
        
        ratable_projects(current)
        removeModal()
        current_step(1)
        showNotification("Import successful!", type = "message")
        
        alt_rating_update()
        rerender_table(rerender_table() + 1)
        showNotification("Data updated!")
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
      fields_needed <- c(
        "project_id",
        "organization_name",
        "project_name",
        "met_hud_thresholds",
        "met_coc_thresholds",
        "weighted_score"
      )
      
      tagList(
        h4("Map Fields"),
        layout_columns(
          strong("Fields in ORR"),
          strong("Fields in Upload"),
          col_widths = c(6, 6)
        ),
        
        lapply(fields_needed, function(field) {
          layout_columns(
            div(field),
            selectInput(
              ns(paste0("map_", field)),
              label = NULL,
              choices = c("Choose one" = "", colnames(rv_uploaded())),
              selected = if(field %in% colnames(rv_uploaded())) field else ""
            ),
            col_widths = c(6, 6)
          )
        })
      )
    }) # end field_mapping_ui
    
    # --- User PResence ----
    mod_user_presence_server(
      id = "presence",
      user_coc = user_coc,
      # All inputs on this page are tied to the version ID
      record_id = reactive({ input$alternative_rating_table_rows_selected }),
      # Only pulse if the main navbar is on 'funding_priorities'
      active = reactive({ nav_control() == "rating" })
    )
    
  }) # end module server
}
