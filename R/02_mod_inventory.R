mod_inventory_ui <- function(id) {
  ns <- NS(id)
  
  col_names <- get_project_col_names()
  
  nav_panel(
    "Review Projects",
    icon = icon("list-check"),
    value = id,
    card(
      card_header(h4("Projects to be Reviewed")),
      card_body(
        fillable = FALSE,
        min_height = "60vh",
        max_height = "81vh",
        helpText("To edit or update an existing project, double-click into a cell. 
                 The green fields are necessary for using later pages of this tool. To add a project, use the \"Add New Project\" button below. "),
        # This adds selectize dependencies, to avoid conflicts with DT and ensure selectize inputs show up as such
        htmltools::findDependencies(selectizeInput('letters', "letters", choices = letters[1:5])),
        
        dropdownButton(
          inputId = ns("field_display_control"),
          label = "Choose Fields to Display",
          icon = icon("sliders"),
          circle = FALSE,
          
          prettySwitch(ns('toggle_bed_fields'), label = 'Show Bed Inventory Fields', value = TRUE, fill = TRUE, status = 'primary'), 
          pickerInput(
            ns('projects_col_selections'), label = 'Choose Fields to Display',
            choices = setNames(col_names, variable_labels[col_names]),
            selected = col_names,
            multiple = TRUE, 
            
            options = pickerOptions(
              selectedTextFormat = 'count',
              countSelectedText = '{0} Fields Displayed',
              selectAllText = 'Select All',
              deselectAllText = 'De-select All',
              actionsBox = TRUE
            )
          )
        ),
        mod_user_presence_ui(ns("presence")),
        DTOutput(ns("projects_table")) |> shinycssloaders::withSpinner()
        # br(),
        # textOutput(ns("projects_table_counts")),
        # helpText("Note: Projects with funding action \"Ignore\" are filtered out by default.")
      ),
      card_footer(
        actionButton(ns("add_project_btn"), "Add New Project", icon = icon("plus")),
        actionButton(ns("view_giw_btn"), "View GIW Data", icon = icon("table"))
      )
    ),
    absolutePanel(
      id = ns("giw_panel"),
      style = "display:none;",
      card(
        h3("GIW"),
        actionButton(ns("close_giw"), "X", class = "btn-danger btn-sm"),
        p(em("Locate the desired project(s) and copy the grant number into the Inventory")),
        DTOutput(ns("giw_tbl")) |> withSpinner()
      ),
      draggable = TRUE,
      width = "60vw",
      height = "50vh",
      top = "10vh",
      left = "20vw"
    )
  )
}

mod_inventory_server <- function(id, nav_control, user_coc, parent_session, help_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Hardcodes and reactiveValues --------------
    user_columns <- c("dv_renewal", "grant_number", "coc_amount_awarded_last_year", "coc_amount_expended_last_year", "coc_funding_requested", "funding_action")
    funding_columns <- c("coc_amount_awarded_last_year", "coc_amount_expended_last_year", "coc_funding_requested", "amount_other_public_funding", "amount_private_funding")
    renew_only_columns <- c("grant_number", "coc_amount_awarded_last_year", "coc_amount_expended_last_year")
    # Keep track of active observers for Add Project modals
    # Need them in a reactivevalues list so we can destroy them and avoid duplicate ones uncessarilly
    # however, we need to be able to have multiple in case they Add Another project
    modal_observers <- reactiveValues() 
    
    # all CoC version project data
    projects_data <- reactiveVal(NULL)
    
    # binary indicator for whether a new row/project has been added
    is_new_project <- reactiveVal(FALSE)
    
    calculated_cols <- c("ch_bed_inventory", "vet_bed_inventory", "youth_bed_inventory")
    
    # # yhdp info for passing around
    # yhdp_replacement_info <- reactiveValues(
    #   info = NULL,
    #   new_value = NULL,
    #   project_to_replace = NULL,
    #   funding_source = NULL
    # )
    
    updated_col_selections_from_db <- reactiveVal(NULL)
    refresh_trigger <- reactiveVal(0)
    
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
    observeEvent(c(user_coc$coc_version_id, refresh_trigger()), {
      req(user_coc$coc_version_id)
      
      data <- get_coc_projects(user_coc$coc_version_id) |>
        fselect(-coc_version_id, -date_created, -date_updated, -updated_by ) %>% #-amount_other_public_funding, -amount_private_funding) %>% # needs to be %>% instead of |>
        fmutate(
          funding_action = convert_to_factor(., "funding_action"),
          project_type = convert_to_factor(., "project_type"),
          target_population = convert_to_factor(., "target_population"),
          dv_renewal = factor_yesno(dv_renewal),
          mckinneyvento = factor_yesno(mckinneyvento),
          # mckinneyventoyhdp = factor_yesno(mckinneyventoyhdp),
          is_dedicated_ch_fam = factor_yesno(is_dedicated_ch_fam),
          is_dedicated_ch_ind = factor_yesno(is_dedicated_ch_ind),
          is_dedicated_dv = factor_yesno(is_dedicated_dv)
        ) |>
        add_calculated_fields()

      projects_data(data)
      
      cols_to_hide <- get_user_setting(user_coc, "inventory_cols_to_hide")
      cols_to_show <- if(length(cols_to_hide) > 0) {
        setdiff(names(data), c(strsplit(cols_to_hide, ",")[[1]], "version_id", "created_by"))
      } else {
        setdiff(names(data), c("version_id", "created_by"))
      }
      
      updatePickerInput(session, inputId = 'projects_col_selections', selected = cols_to_show)
      updated_col_selections_from_db(TRUE) 
    }, ignoreInit = TRUE)
    
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
      initial_filter[[which(names(data) == "funding_action")]] <- list(search = '["Renew","Reallocate","New","Expand"]') # removed Replace; not happening for FY25

      # helper text explaining this
      helper_html <- "<span title='Projects with funding action \"Ignore\" are filtered out by default.'>funding action ⓘ</span>"
      
      
      # More readable col header text
      colnames <- variable_labels[names(data)]
      colnames["funding_action"] <- helper_html
      colnames <- unname(colnames)
      
      ## initially, only hide pre-specified columns; later, will hide user settings-based ones
      cols_to_hide <- setdiff(names(data), isolate(input$projects_col_selections)) |> append(c("version_id", "created_by"))
      
      funding_action_idx <- match("funding_action", names(data)) - 1
      grant_number_idx <- match("grant_number", names(data)) - 1
      
      ## Call inline-editable table function ---------
      initialize_inline_edit_table_ui(
        data,
        initial_filter = initial_filter,
        column_defs = list(
          list(
            targets = which(names(data) %in% cols_to_hide) - 1, 
            className = "hidden",
            visible = FALSE
          ),
          list(
            targets = which(names(data) %in% renew_only_columns) - 1,
            render = JS(glue::glue(
              "function(data, type, row, meta) {{
                if(type != 'display') return data;
                
                if (row === undefined) return data;
                if (row[{funding_action_idx}] == 'New') return 'N/A';
                if (data === null) return '';
                if (meta.col == {grant_number_idx}) return data;
                
                // Manual currency formatting: $1,234.56
                // Ensure data is numeric before calling toFixed
                var num = Number(data);
                if (isNaN(num)) return data; 
                return '$' + num.toFixed(2).replace(/\\d(?=(\\d{3})+\\.)/g, '$&,');
              }}"
            )),
            createdCell = JS(glue::glue(
              "function(td, cellData, rowData, row, col) {{
                // Disable N/As
                if (td.innerText == 'N/A') {{
                  $(td).css('pointer-events', 'none');      // Disables clicks/editing
                  $(td).css('color', '#a0a0a0');            // Grays out text
                  $(td).css('font-style', 'italic');        // Optional
                }}
              }}"
            ))
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
            columns = c("ch_beds_hh_w_only_children", "total_ch_ind_beds"),
            `min-width` = "110px"
          ),
          function(x) formatStyle(
            x,
            columns = user_columns,
            backgroundColor = USER_ENTRY_BG_COLOR
          ),
          # Replacement projects should fill out these fields, and thus color them green.
          # function(x) formatStyle(
          #   x,
          #   columns = c("project_name","project_type","par_youth_beds","single_youth_beds"),            # what to style
          #   valueColumns = c("funding_action"),
          #   backgroundColor = styleEqual("Replace", USER_ENTRY_BG_COLOR)
          # ),
          function(x) formatStyle(
            x,
            columns = c("all_ind_beds"),            # what to style
            valueColumns = c("created_by"),
            backgroundColor = styleEqual(SERVICE_ACCOUNT, 'var(--bs-body-bg)', default = 'lightgray'),
            pointerEvents = styleEqual(SERVICE_ACCOUNT, 'auto', default = 'none')
          ),
          function(x) formatCurrency(
            x, 
            columns = setdiff(funding_columns, renew_only_columns),
            currency = "$", 
            digits = 0
          ),
          
          function(x) formatRound(
            x,
            columns = grep('bed', names(data)),
            digits = 0
          )
        ),
        colnames = colnames,
        cols_to_disable = c(calculated_cols, "dv_fam_beds","dv_ind_beds"),
        options = list(
          paging = TRUE,
          pageLength = 100,
          
          # Letter	Meaning
          # l	Length changing input (rows per page selector)
          # f	Filtering input (search box)
          # r	Processing display element (shows “Processing…” when loading)
          # t	The table itself
          # i	Table information summary
          # p	Pagination controls
          # B	Buttons (CSV, Excel, PDF, etc.)
          dom = 'frtip'
        )
      )
    }) # end project_Table renderDT
    
    ## datatable proxy-----
    # By updating a proxy (via `replaceData`), updates are faster and don't "flicker" the table
    # However it doesn't work when adding new rows
    projects_table_proxy <- dataTableProxy("projects_table",session = session)
    projects_table_proxy$id <- "projects_table" # setting id to raw_id seems to fix auto-scrolling
    
    observe({
      req(projects_data())
      replaceData(projects_table_proxy, projects_data(), resetPaging = FALSE, rownames = FALSE)
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
      } #else if(val == "Replace") {
      #   if(project_data$mckinneyventoyhdp != "Yes") {
      #     showNotification(
      #       "It looks like you are trying to replace a non-YHDP project. Only 
      #       YHDP projects can be replaced. If this is a YHDP project, 
      #       please mark the McKinney- Vento: YHDP field as 'Yes'"
      #     )
      #     return(FALSE)
      #   }
      # }
      return(TRUE)
    }
    
    # Update DT table with Bed Inventory fields (switchInput)
    observeEvent(input$toggle_bed_fields, {
      req(user_coc$auth)
      req(!is.null(user_coc$coc_version_id) & nav_control() == 'inventory')
      req(projects_data())
      
      bed_fields <- grep('bed', names(projects_data()))
      bed_field_names <- names(projects_data())[bed_fields]
      
      if(input$toggle_bed_fields){
        updatePickerInput(session, inputId = 'projects_col_selections', selected = union(input$projects_col_selections, bed_field_names))
      } else {
        updatePickerInput(session, inputId = 'projects_col_selections', selected = setdiff(input$projects_col_selections, bed_field_names))
      }
    }, ignoreInit = TRUE)
    
    # Update DT table with column changes made in dropdown (pickerInput)
    observeEvent(input$projects_col_selections, {
      req(user_coc$auth)
      req(!is.null(user_coc$coc_version_id) & nav_control() == 'inventory')
      req(projects_data())
      
      colnames_to_hide <- setdiff(names(projects_data()), c(input$projects_col_selections, "version_id", "created_by"))
      cols_to_hide <- match(colnames_to_hide, names(projects_data())) - 1
      cols_to_show <- match(input$projects_col_selections, names(projects_data())) - 1
      # or, equivalently: cols_to_show <- match(input$projects_col_selections, names(projects_data())) - 1
      
      ## show and hide columns as needed
      projects_table_proxy$id <- ns("projects_table")
      if(length(cols_to_hide) > 0) {
        hideCols(projects_table_proxy, hide = cols_to_hide)
      }
      if(length(cols_to_show) > 0) {
        showCols(projects_table_proxy, show = cols_to_show)
      }
      projects_table_proxy$id <- "projects_table"
      
      update_user_coc_setting(user_coc, "inventory_cols_to_hide", colnames_to_hide)
    }, ignoreInit = TRUE)
    
    # Update projects -----
    ## consolidated update function
    inventory_update <- function(info, value) {
      project_data <- projects_data()[project_id == info$project_id]
      proj_id <- as.character(project_data$project_id)
      col_name <- colnames(projects_data())[info$col + 1]
      
      # We send info$value, which is the user-friendly text ("Reallocate", "Yes", etc.)
      needs_refresh <- update_inventory_db(value, col_name, proj_id, project_data$version_id)
      if(!needs_refresh) {
        user_coc$projects_updated <- user_coc$projects_updated + 1
        update_datatable(proj_id, col_name, info$value)
      } else {
        refresh_trigger(refresh_trigger() + 1)
      }
    }
    
    update_datatable <- function(proj_id, col_name, value) {
      # update the reactiveVal that updates the proxy
      updated_data <- copy(projects_data())[
        project_id == proj_id, 
        c(col_name, "version_id") := list(value, version_id + 1)
      ] |>
        add_calculated_fields()
      
      if(col_name == "funding_action" && value == "New") {
        updated_data <- updated_data |>
          fmutate(
            grant_number = NA,
            coc_amount_awarded_last_year = NA,
            coc_amount_expended_last_year = NA
          )
      }
      
      projects_data(updated_data)
    }
    
    # Append project -----
    ## consolidated append
    inventory_append <- function(new_project_data) {
      append_to_datatable(new_project_data)
      s <- append_inventory_to_db(new_project_data)
      
      if(isTruthy(s))
        user_coc$projects_updated <- user_coc$projects_updated + 1
      #showNotification("Project submitted successfully.", type = "message")
    }
    
    append_to_datatable <- function(new_project_data) {
      new_row <- new_project_data |> 
        fmutate(
          project_id = as.integer(fmax(projects_data()$project_id) + 1),
          mckinneyvento = factor_yesno(mckinneyvento),
          # mckinneyventoyhdp = factor_yesno(mckinneyventoyhdp),
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
      
      cols <- intersect(names(projects_data()), names(new_row))
      new_row <- new_row[, cols, with = FALSE]
      
      projects_data(
        rowbind(projects_data(), new_row, fill = TRUE) |> roworderv(neworder = fnrow(projects_data()) + 1)
      )
      
      is_new_project(TRUE)
    }
    
    append_inventory_to_db <- function(new_project_data) {
      db_data <- new_project_data |>
        fmutate(
          coc_version_id = user_coc$coc_version_id,
          funding_action = get_lookup_refid(funding_action, "funding_action"),
          project_type = get_lookup_refid(project_type, "project_type"),
          target_population = get_lookup_refid(target_population, "target_population")
        )
      
      db_append("projects", db_data)
    }
    
    # Main inline-cell edit event -----
    observeEvent(input$projects_table_cell_edit, {
      req(projects_data())
      
      info <- input$projects_table_cell_edit
      
      req(!identical(info$value, info$oldValue))
      
      col_name <- colnames(projects_data())[info$col + 1]
      
      if(col_name == "grant_number") {
        if (nchar(info$value) > 15) {
          showNotification("Grant number cannot be longer than 15 characters.", type = "error")
          revert_cell(ns("projects_table"), info, input$projects_table_rows_current, projects_data())
          return()
        }
      }
      
      # numeric validation
      if (is.numeric(projects_data()[[col_name]])) {
        is_valid <- validate_numeric_entry(projects_data(), col_name, info$value)
        if(!is_valid) revert_cell(ns("projects_table"), info, input$projects_table_rows_current, projects_data())
        req(is_valid)
      }
      
      info$project_id <- ifelse(
        is.na(info$project_id) || is.null(info$project_id),
        as.character(projects_data()[info$row, "project_id"]),
        info$project_id
      )
      
      project_data <- projects_data()[project_id == info$project_id]
      
      funding_source <- ifelse(
        # project_data$mckinneyventoyhdp == "Yes",
        # "YHDP",
        # ifelse(
          isTruthy(project_data$dv_renewal == "Yes" || project_data$target_population == "DV"),
          "DV",
          "CoC"
        # )
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
      if(col_name == "funding_action" && info$value %in% c("Reallocate")) { # removed "Replace". not happening in FY25
        # Validity check ----
        is_valid <- validity_pre_checks(project_data, funding_source, info$value)
        
        # If they can't Reallocate or Replace, bring back old value in table cell
        if(!is_valid) revert_cell(ns("projects_table"), info, input$projects_table_rows_current, projects_data())
        req(is_valid)
        
        ## YHDP Replacement -----
        # if(info$value == "Replace") {
        #   # Update reactiveValues so it's visible to observeEvents of the confirmation pop-up
        #   yhdp_replacement_info$funding_source <- funding_source
        #   yhdp_replacement_info$info <- info
        #   yhdp_replacement_info$new_value <- new_value
        #   yhdp_replacement_info$project_to_replace <- project_data
        #   
        #   showModal(
        #     modalDialog(
        #       title = "YHDP Replacement Confirmation",
        #       HTML("
        #           Are you replacing this project with multiple projects? <br><br>
        #           If not, click 'No'. Then update the newly highlighted fields 
        #           for the project. Note that because this is a YHDP Replacement 
        #           project, you can only edit the Project Name, Project Type, and Youth 
        #           bed fields. <br><br> If you are Replacing this project with 
        #           multiple projects, click 'Yes', then you will need to create new 
        #           projects as well as editing this row of the List of Project tab. 
        #           First enter the additional project's information in the pop-up after 
        #           you click 'Yes'. If you are creating more than two projects to 
        #           Replace the current project, then click on the 'additional 
        #           replacement project?' link at the bottom right corner of the pop-up. 
        #           When you are finished adding additional projects, which will appear 
        #           in the list on this tab, then return to the row of the of the current 
        #           project that is being Replaced and update the highlighted fields.
        #         "),
        #       footer = tagList(
        #         actionButton(ns('replace_multiple'), label="Yes"),
        #         actionButton(ns('replace_one'), label="No"),
        #         actionButton(ns('replace_cancel'), label="Cancel"),
        #       ),
        #       size = "l"
        #     )
        #   ) # end showModal
        # } 
        ## Reallocation -----
        # else {
          launch_modal(paste0(funding_source, " Reallocation"), funding_source)
        # }
      } # end reallocation/replace IF-block
      # Update after non-reallocation and non-replace ------
      inventory_update(info, new_value)
    }, ignoreInit = TRUE)
    
    modal_trigger <- reactiveVal(0)
    current_form_type <- reactiveVal("New")
    current_funding_src <- reactiveVal("")
    # current_proj_to_replace <- reactiveVal(NULL)
    
    # Handle replacement modal ------
    ## User wants to replace with multiple projects ----
    # observeEvent(input$replace_multiple, {
    #   removeModal()
    #   launch_modal(
    #     type ="YHDP Replacement", 
    #     source = yhdp_replacement_info$funding_source, 
    #     replacement = yhdp_replacement_info$project_to_replace
    #   )
    # })
    # 
    #     ## User wants to replace with one project ----
    # observeEvent(input$replace_one, {
    #   removeModal()
    # })
    # 
    # ## User cancelled replacement ----
    # observeEvent(input$replace_cancel, {
    #   revert_cell(ns("projects_table"), yhdp_replacement_info$info, input$projects_table_rows_current, projects_data())
    #   removeModal()
    #   # no need to do inventory_update because we haven't modified the db or datatable yet
    # })
    
    orgnames <- reactive({
      req(user_coc$coc_version_id)
        c("Select or add Organization" = "", funique(projects_data()$organization_name, sort=TRUE))
    })
    
    # Project modal control -------------
    # Adding it once here and managing its status
    modal_submission <- mod_inventory_add_project_server(
      id = "add_project",
      trigger = modal_trigger,
      form_type = current_form_type,
      funding_source = current_funding_src,
      # project_to_replace = current_proj_to_replace,
      user_coc = user_coc,
      orgnames = orgnames
    )
    
    # 2. Define a single UI launcher
    launch_modal <- function(type, source = "", replacement = NULL) {
      current_form_type(type)
      current_funding_src(source)
      # current_proj_to_replace(replacement)
      
      # Increment trigger to tell child server to prepopulate/reset
      modal_trigger(modal_trigger() + 1) 
      
      showModal(mod_inventory_add_project_ui(ns("add_project"), orgnames = orgnames()))
    }
    
    # Handle user's add project submission
    observeEvent(modal_submission$status, {
      req(modal_submission$status)
      req(modal_submission$status != "error")
      
      inventory_append(modal_submission$project_data)
    }, ignoreNULL = TRUE)
    
    # Add additional project handling ----
    observeEvent(input$add_project_btn, {
      print('clicked add_project_btn')
      launch_modal("New")
    })
    
    # View GIW Data -------
    giw_data <- reactive({
      req(user_coc$auth)
      get_db_tbl("giw")[coc == user_coc$coc]
    })
    
    observeEvent(input$view_giw_btn, {
      shinyjs::show("giw_panel")
    })
    observeEvent(input$close_giw, {
      shinyjs::hide("giw_panel")
    })
    
    record_being_edited <- reactiveVal(NULL)
    observeEvent(input$projects_table_cell_being_edited, {
      record_being_edited(
        list(
          record_id = projects_data()[input$projects_table_cell_clicked$row]$project_id,
          field = names(projects_data())[[input$projects_table_cell_clicked$col + 1]]
        )
      )
    })
    
    mod_user_presence_server(
      id = "presence", # Internal ID for this leaf module
      user_coc = user_coc,
      # We use the project ID because we are rating a specific project
      record_id = reactive({ record_being_edited()$record_id }), 
      field = reactive({ record_being_edited()$field }),
      active = reactive({ nav_control() == "inventory"})
    )
    
    # output$projects_table_counts <- renderText({
    #   req(projects_data())
    #   paste0("Showing ", length(input$projects_table_rows_current), " projects (out of ", fnrow( projects_data()), " total projects)")
    # })
    output$giw_tbl <- renderDT({
      data <- giw_data() |>
        fselect(-date_created, -date_updated, -created_by, -updated_by, -version_id)
      
      names(data) <- variable_labels[match(names(data), names(variable_labels))]
      
      datatable(
        data,
        fillContainer = TRUE,
        selection = "none",
        options = list(
          pageLength = 200,
          scrollY = "500px",
          columnDefs = list(
            list(visible = FALSE, targets = 0) # 0 = first column (0-based index)
          ),
          language = list(
            zeroRecords = "No GIW data available for this CoC."
          )
        ),
        lazyRender = TRUE,
        filter = 'top'
      )
    })
    
  }) # end moduleServer
}
