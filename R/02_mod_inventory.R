mod_inventory_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    "Review Projects",
    icon = icon("list-check"),
    value = id,
    card(
      card_header(h4("Projects to be Reviewed")),
      card_body(
        fillable = FALSE,
        min_height = "60vh",
        max_height = "76vh",
        helpText("To edit or update an existing project, double-click into a cell. 
                 The green fields are necessary for using later pages of this tool. To add a project, use the \"Add New Project\" button below. "),
        DTOutput(ns("projects_table")),#|> shinycssloaders::withSpinner()
        br(),
        textOutput(ns("projects_table_counts"))
      ),
      card_footer(
        actionButton(ns("add_project_btn"), "Add New Project", icon = icon("plus")),
        actionButton(ns("view_giw_btn"), "View GIW Data", icon = icon("table"))
      )
    )
  )
}

mod_inventory_server <- function(id, nav_control, user_coc, parent_session, module_returns) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Hardcodes and reactiveValues --------------
    user_columns <- c("dv_renewal", "grant_number", "coc_amount_awarded_last_year", "coc_amount_expended_last_year", "coc_funding_requested", "funding_action")
    funding_columns <- c("coc_amount_awarded_last_year", "coc_amount_expended_last_year", "coc_funding_requested")
    
    # Keep track of active observers for Add Project modals
    # Need them in a reactivevalues list so we can destroy them and avoid duplicate ones uncessarilly
    # however, we need to be able to have multiple in case they Add Another project
    modal_observers <- reactiveValues() 
    
    # all CoC version project data
    projects_data <- reactiveVal(NULL)
    
    # binary indicator for whether a new row/project has been added
    is_new_project <- reactiveVal(FALSE)
    
    # yhdp info for passing around
    yhdp_replacement_info <- reactiveValues(
      info = NULL,
      new_value = NULL,
      project_to_replace = NULL,
      funding_source = NULL
    )
    
    # Add fields only displayed in Inventory
    add_calculated_fields <- function(project_data, is_new = FALSE) {
      project_data <- project_data |>
        fmutate(
          ch_bed_inventory = ch_fam_beds + total_ch_ind_beds,
          vet_bed_inventory = vet_fam_beds + vet_ind_beds,
          youth_bed_inventory = par_youth_beds + single_youth_beds
        )

      # New/Additional projects have some fields set, not calculated
      if(!is_new) {
        project_data <- project_data |>
          fmutate(
            all_ind_beds = beds_hh_wo_children + beds_hh_w_only_children,
            total_ch_ind_beds = ch_beds_hh_wo_children + ch_beds_hh_w_only_children
          )
      }
      return(project_data)
    }
    
    # Initialize projects_data ------
    observe({
      req(user_coc$coc_version_id)

      data <- get_db_query(
        "SELECT * FROM projects WHERE coc_version_id = $1", 
        params = user_coc$coc_version_id
      ) |>
        fselect(-coc_version_id, -date_created, -date_updated, -updated_by, -amount_other_public_funding, -amount_private_funding) %>% # needs to be %>% instead of |>
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
        ) |>
        add_calculated_fields()

      projects_data(data)
    })
    
    # Projects datatable -----
    output$projects_table <- renderDT({
      req(user_coc$coc_version_id)
      
      if(is_new_project()) {
        data <- projects_data()
        is_new_project(FALSE)
      } else {
        data <- isolate(projects_data())
      }
      shiny::validate(need(
        nrow(data) > 0, 
        "No rows"
      ))
      
      ## filter out Ignores by default-----
      initial_filter <- vector("list", ncol(data))
      initial_filter[[which(names(data) == "funding_action")]] <- list(search = '["Renew","Reallocate","Replace","New","Expand"]')

      colnames <- unname(project_variable_labels[names(data)])
      
      ## Call inline-editable table function ---------
      initialize_inline_edit_table_ui(
        data,
        tableID = ns("projects_table"), 
        initial_filter = initial_filter,
        column_defs = list(
          list(
            targets =c(which(names(data) == "created_by") - 1,
                       which(names(data) == 'geocode') - 1), 
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
            columns = c(
              "coc_amount_awarded_last_year", 
              "coc_amount_expended_last_year", 
              "is_dedicated_ch_fam",
              "is_dedicated_ch_ind"
            ),
            `min-width` = "150px"
          ),
          function(x) formatStyle(
            x,
            columns = c(
              "ch_beds_hh_w_only_children",
              "total_ch_ind_beds"
            ),
            `min-width` = "110px"
          ),
          function(x) formatStyle(
            x,
            columns = user_columns,
            backgroundColor = USER_ENTRY_BG_COLOR
          ),
          # Replacement projects should fill out these fields, and thus color them green.
          function(x) formatStyle(
            x,
            columns = c("project_name","project_type","par_youth_beds","single_youth_beds"),            # what to style
            valueColumns = c("funding_action"),
            backgroundColor = styleEqual("Replace", USER_ENTRY_BG_COLOR)
          ),
          function(x) formatStyle(
            x,
            columns = c("all_ind_beds"),            # what to style
            valueColumns = c("created_by"),
            backgroundColor = styleEqual(SERVICE_ACCOUNT, 'var(--bs-body-bg)', default = 'lightgray'),
            pointerEvents = styleEqual(SERVICE_ACCOUNT, 'auto', default = 'none')
          ),
          function(x) formatCurrency(
            x, 
            columns = funding_columns, 
            currency = "$", 
            digits = 0
          )
        ),
        colnames = colnames,
        cols_to_disable = c("ch_bed_inventory", "vet_bed_inventory","youth_bed_inventory", "dv_fam_beds","dv_ind_beds"),
        buttons = list(
          list(
            extend = 'collection',
            text="Show/Hide Bed Inventory",
            action = DT::JS(sprintf("
              function ( e, dt, node, config ) {
                var cols = %s;
                dt.columns(cols).visible(!dt.column(cols[0]).visible());
              }",
              jsonlite::toJSON(grep("Bed", colnames) - 1)
            ))
          )
        )
      )
    })
    
    ## datatable proxy-----
    # By updating a proxy (via `replaceData`), updates are faster and don't "flicker" the table
    # However it doesn't work when adding new rows
    projects_table_proxy <- dataTableProxy(ns("projects_table"))
    
    observe({
      req(projects_data())
      replaceData(projects_table_proxy, projects_data(), resetPaging = FALSE)
    })
    
    # Checks whether value is valid
    validity_pre_checks <- function(project_data, funding_source, val) {
      if(val == "Reallocate") {
        if(funding_source == "DV" && project_data$project_type == "SSO - CE") {
          showNotification(
            "According to the FY2026 NOFO, you cannot reallocate a DV SSO-CE 
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
    ## consolidated update function
    inventory_update <- function(info, value) {
      project_data <- projects_data()[project_id == info$project_id]
      proj_id <- as.character(project_data$project_id)
      col_name <- colnames(projects_data())[info$col + 1]
      
      # We send info$value, which is the user-friendly text ("Reallocate", "Yes", etc.)
      update_datatable(proj_id, col_name, info$value)
      update_db(value, col_name, proj_id)
    }
    
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
      updated_data <- copy(projects_data())[
        project_id == proj_id, 
        (col_name) := value
      ] |>
        add_calculated_fields()
      
      projects_data(updated_data)
    }
    
    # Append project -----
    ## consolidated append
    inventory_append <- function(new_project_data) {
      append_to_datatable(new_project_data)
      append_to_db(new_project_data)
      
      #showNotification("Project submitted successfully.", type = "message")
    }
    
    append_to_datatable <- function(new_project_data) {
      new_row <- new_project_data |> 
        fmutate(
          project_id = as.integer(fmax(projects_data()$project_id) + 1),
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
        ) |>
        add_calculated_fields(TRUE)
      
      projects_data(
        rbind(new_row, copy(projects_data()), fill=TRUE)
      )
      
      is_new_project(TRUE)
    }
    
    append_to_db <- function(new_project_data) {
      db_data <- new_project_data |>
        fmutate(
          coc_version_id = user_coc$coc_version_id,
          funding_action = get_lookup_refid(funding_action, "funding_action"),
          project_type = get_lookup_refid(project_type, "project_type"),
          target_population = get_lookup_refid(target_population, "target_population"),
          date_created = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        )
      
      DBI::dbAppendTable(DB_CON, "projects", db_data)
    }
    
    # Main inline-cell edit event -----
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
      
      funding_source <- ifelse(
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
          LOOKUPS[reference_type == col_name & value == info$value]$reference_id
        ),
        info$value
      )
      
      ## Handle reallocation and replace -----
      if(col_name == "funding_action" && info$value %in% c("Reallocate","Replace")) {
        # Validity check ----
        is_valid <- validity_pre_checks(project_data, funding_source, info$value)
        
        # If they can't Reallocate or Replace, bring back old value in table cell
        if(!is_valid) revert_cell(info)
        req(is_valid)
        
        ## YHDP Replacement -----
        if(info$value == "Replace") {
          # Update reactiveValues so it's visible to observeEvents of the confirmation pop-up
          yhdp_replacement_info$funding_source <- funding_source
          yhdp_replacement_info$info <- info
          yhdp_replacement_info$new_value <- new_value
          yhdp_replacement_info$project_to_replace <- project_data
          
          showModal(
            modalDialog(
              title = "YHDP Replacement Confirmation",
              HTML("
                  Are you replacing this project with multiple projects? <br><br>
                  If not, then click 'No'. Then update the newly highlighted fields 
                  for the project. Note that because this is a YHDP Replacement 
                  project, you can only edit the Project Name, Project Type, and Youth 
                  bed fields. <br><br> If you are Replacing this project with 
                  multiple projects, click 'Yes', then you will need to create new 
                  projects as well as editing this row of the List of Project tab. 
                  First enter the additional project's information in the pop-up after 
                  you click 'Yes'. If you are creating more than two projects to 
                  Replace the current project, then click on the 'additional 
                  replacement project?' link at the bottom right corner of the pop-up. 
                  When you are finished adding additional projects, which will appear 
                  in the list on this tab, then return to the row of the of the current 
                  project that is being Replaced and update the highlighted fields.
                "),
              footer = tagList(
                actionButton(ns('replace_multiple'), label="Yes"),
                actionButton(ns('replace_one'), label="No"),
                actionButton(ns('replace_cancel'), label="Cancel"),
              )
            )
          ) # end showModal
        } 
        ## Reallocation -----
        else {
          form_type <- paste0(funding_source, " Reallocation")
          show_project_modal(form_type, funding_source, info, new_value)
        }
      }
      # Update after non-reallocation and non-replace ------
      else {
        inventory_update(info, new_value)
      }
    }, ignoreInit = TRUE)
    
    # Handle replacement modal ------
    ## Revert cell to original value -----
    revert_cell <- function(info) {
      shinyjs::runjs(sprintf(
        "
              var table = $('#%s table').DataTable();
              table.cell(%s, %s).data('%s');
            ", 
        ns("projects_table"), info$row - 1, info$col, info$oldValue
      ))
    }
    
    ## User wants to replace with multiple projects ----
    observeEvent(input$replace_multiple, {
      show_project_modal(
        "YHDP Replacement", 
        yhdp_replacement_info$funding_source, 
        yhdp_replacement_info$info, 
        yhdp_replacement_info$new_value,
        yhdp_replacement_info$project_to_replace
      )
    })
    
    ## User wants to replace with one project ----
    observeEvent(input$replace_one, {
      removeModal()
      inventory_update(yhdp_replacement_info$info, yhdp_replacement_info$new_value)
    })
    
    ## User cancelled replacement ----
    observeEvent(input$replace_cancel, {
      revert_cell(yhdp_replacement_info$info)
      removeModal()
      # no need to do inventory_update because we haven't modified the db or datatable yet
    })
    
    # Project modal control -------------
    # A function to show the modal and set up the server logic
    show_project_modal <- function(form_type = "New", funding_source = "", info = NULL, new_value = NULL, project_to_replace = NULL, observer_id = NULL) {
      if (is.null(observer_id)) {
        # Generate a unique ID for this observer version
        observer_id <- paste0("modal_obs_", digest::digest(runif(1)))
      }
      
      shiny::invalidateLater(100)
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
        funding_source = funding_source,
        user_coc = user_coc,
        parent_session = session
      )

      # Create the observer and store it
      modal_observers[[observer_id]] <- observeEvent(modal_submission$status, {
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
          show_project_modal(form_type, funding_source, info, new_value, project_to_replace)
        } else {
          # If not adding another, the modal chain is done, destroy this observer
          if (!is.null(modal_observers[[observer_id]])) {
            modal_observers[[observer_id]]$destroy()
            modal_observers[[observer_id]] <- NULL # Remove from reactiveValues
          }
        }
      }, ignoreNULL = TRUE, once = FALSE) # keep once=FALSE because it might trigger multiple times for "add another"
    }
    
    # Add additional project handling ----
    observeEvent(input$add_project_btn, {
      show_project_modal()
    })
    
    # View GIW Data -------
    giw_data <- reactive({
      req(user_coc$auth)
      get_db_tbl("giw")[coc == user_coc$coc]
    })
    
    observeEvent(input$view_giw_btn, {
      req(isTruthy(giw_data()))
      
        data <- giw_data() |>
          fselect(-date_created, -date_updated, -created_by, -updated_by)
      
      names(data) <- giw_variable_labels[match(names(data), names(giw_variable_labels))]
      
      showModal(
        modalDialog(
          title = "GIW",
          p(em("Locate the desired project(s) and copy the grant number into the Inventory")),
          DT::renderDT(
            data,
            fillContainer = TRUE,
            options = list(
              pageLength = 200,
              scrollY = "400px",
              columnDefs = list(
                list(visible = FALSE, targets = 0)  # 0 = first column (0-based index)
              ),
              language = list(
                zeroRecords = "No GIW data available for this CoC."
              )
            )
          ),
          size="xl",
          easyClose = TRUE
        )
      )
    })
    
    output$projects_table_counts <- renderText({
      req(projects_data())
      paste0("Showing ", length(input$projects_table_rows_current), " projects (filtered from ", fnrow( projects_data()), " total projects)")
    })
    
  }) # end moduleServer
}
