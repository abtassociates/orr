mod_coc_selection_ui <- function(id) {
  ns <- NS(id)

  card(id = id,
    card_header(h4("Versions")),
    card_body(
      fillable = FALSE,
      p('A CoC can have multiple versions of its ORR. Versions can be created to test different combinations of factors and parameters. To create your own ORR version, click "Create New Version". To create a copy of an existing version, select the version in the table below and click "Copy Version".'),
      p('Multiple users can work together on the same ORR. To collaborate on an existing ORR version made by another user, click "Request Access to a CoC".'),
      # a "Create" button or link above the table will display so they can create a new CoC Version
      DTOutput(ns('coc_versions_dt'),fill = F) |> shinycssloaders::withSpinner(),
      actionButton(ns('create_new_version'), "Create New Version", icon = icon('circle-plus'), class='btn-primary'),
      actionButton(ns('edit_coc_version'),"Edit Selected Version", icon = icon('edit'), class='btn-secondary'),
      actionButton(ns('delete_coc_version'), "Delete Selected Version", icon = icon('trash'), class='btn-danger'),
      actionButton(ns('copy_version'), "Copy Version", icon = icon('copy'), class="btn-info"),
      actionButton(ns('request_access_direct'), "Request Access to a CoC", icon = icon('unlock'), class="btn-warning")
    )
  )
}

mod_coc_selection_server <- function(id, nav_control, user_coc, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ## versions associated with logged in user
    users_versions <- reactiveVal(NULL)
    
    ## session variables used for sending access requests
    admin_email <- reactiveVal(NULL)
    coc_requested <- reactiveVal(NULL)
    
    all_versions_and_users <- reactiveVal()
    
    refresh_trigger <- reactiveValues(
      versions = 0,
      request_sent = 0,
    )
    
    observeEvent(c(user_coc$auth, refresh_trigger$versions), {
      req(user_coc$auth)
      all_versions_and_users(
        get_all_coc_versions_and_users()
      )
      
      users_versions(
        all_versions_and_users() |>
          fsubset(username == user_coc$username, -username)
      )
    })
    
    project_ids <- reactive({
      req(user_coc$coc_version_id)
      get_coc_projects(user_coc$coc_version_id)$project_id
    })
    
    owner_role_refid <- get_lookup_refid("Owner", "coc_version_role")
    
    ####
    # CoC Versions table ------------------
    ####
    output$coc_versions_dt <- renderDT({
      req(user_coc$auth)
      
      data <- users_versions() |> fselect(-version_id)
      datatable(data, 
                colnames = unname(variable_labels[names(data)]),
                rownames = FALSE,
                options = list(
                  dom = 'ftip', 
                  autowidth = FALSE,
                  columnDefs = list(
                    list(targets=0, className = "hidden")
                  )
                ),
                editable = FALSE,
                style = 'default',
                selection = 'single'
      ) %>% 
        formatDate(
          columns = c('date_created', 'date_updated'),
          method = 'toLocaleString'
        )
    }, server = FALSE)
    
    ####
    # Selected CoC Version Actions --------------
    ####
    requests_by_user <- reactive({
      req(refresh_trigger$request_sent)

      get_all_requests_by_user(user_coc$username) |>
        fsubset(created_by == user_coc$username, coc_version_id, request_status)
    })
    
    observe({
      req(user_coc$auth)
      req(all_versions_and_users(), users_versions(), requests_by_user())
      
      # If there are any versions NOT associated with the current user, allow them to Request Access
      versions_not_associated_with_user <- fsubset(
        all_versions_and_users(),
        !coc_version_id %in% c(users_versions()$coc_version_id, requests_by_user()$coc_version_id)
      )
        
      shinyjs::toggle(id = 'request_access_direct', condition = fnrow(versions_not_associated_with_user) > 0)
    })
    
    ## Selecting a version ------------
    observeEvent(input$coc_versions_dt_rows_selected, {
      current_coc_info <- users_versions()[input$coc_versions_dt_rows_selected, .(coc, coc_version_id, coc_version_role)]
      user_coc$coc <- current_coc_info$coc
      user_coc$coc_version_id <- current_coc_info$coc_version_id
      user_coc$date_updated <- current_coc_info$date_updated
      
      # toggle Inventory tab if they have any versions selected
      toggle_navs_on_coc_selection()
      
      shinyjs::toggle(id = 'edit_coc_version', condition = length(input$coc_versions_dt_rows_selected) > 0)
      shinyjs::toggle(id = 'delete_coc_version', condition = length(input$coc_versions_dt_rows_selected) > 0 && current_coc_info$coc_version_role == "Owner")
      shinyjs::toggle(id = 'copy_version', condition = length(input$coc_versions_dt_rows_selected) > 0)
    }, ignoreNULL = FALSE)
    
    ### toggle navs on version selection ----------------
    toggle_navs_on_coc_selection <- function() {
      for(t in TABS_AFTER_COC_SELECTION) {
        show <- length(input$coc_versions_dt_rows_selected) > 0
        if(t %in% c("rating","ranking")) show <- show && length(project_ids()) > 0
        
        if(show)
          nav_show("nav", target = t, session = parent_session)
        else
          nav_hide("nav", target = t, session = parent_session)
      }
    }
    
    ## Edit version ----------------
    observeEvent(input$edit_coc_version, {
      req(user_coc$auth)
      
      update_coc_version(
        params = list(
          get_lookup_refid("In Progress", "coc_status"), 
          user_coc$username, 
          user_coc$coc_version_id #, 
          # users_versions()[input$coc_versions_dt_rows_selected]$version_id
        )
      )
      
      nav_control("inventory")
    })
    
    ## Delete version ---------------
    observeEvent(input$delete_coc_version, {
      if(users_versions()[input$coc_versions_dt_rows_selected, coc_version_role] == "Owner"){
        
        showModal(
          modalDialog(
            title = 'Confirm Deletion',
            helpText("Are you sure you want to delete this CoC version? This action cannot be undone."),
            footer = tagList(
              actionButton(ns('confirm_deletion'), label='Confirm', icon = icon('trash'), class='btn-danger'),
              modalButton(label='Cancel')
            )
          )
        )
      } else {
        showModal(
          modalDialog(
            title = 'Deletion Not Authorized',
            helpText("You cannot delete a CoC Version as an Editor. If you would like to have this CoC version deleted, please reach out to the Owner of this version."),
            footer = tagList(
              modalButton(label='OK')
            )
          )
        )
      }
    })
    
    observeEvent(input$confirm_deletion, {
      delete_coc_version(user_coc$coc_version_id)
      removeModal()
      refresh_trigger$versions <- refresh_trigger$versions + 1
    })
    
    ## Copy selected version ------------
    observeEvent(input$copy_version, {
      req(input$coc_versions_dt_rows_selected)
      current_version_name <- users_versions()[input$coc_versions_dt_rows_selected, .(coc_version_name)]
      showModal(
        modalDialog(
          title = paste0('Copy ', current_version_name),
          textInput(
            ns("copy_version_name"), 
            "Version Name",
            value = paste0(current_version_name, "_v2")
          ),
          footer = tagList(
            actionButton(ns('copy_orr_confirm'), label="Confirm", class="btn-primary"),
            modalButton(label="Cancel")
          ),
          easyClose = TRUE
        ),
        session = session
      )
    })
    
    observeEvent(input$copy_orr_confirm, {
      pool::poolWithTransaction(get_db_pool(), function(p) {
        coc_version_id <- create_new_version_for_user(
          p, 
          users_versions()[input$coc_versions_dt_rows_selected] |>
            fmutate(coc_version_name = input$copy_version_name) |>
            fselect(-coc_version_role, -date_updated, -date_created)
        )
      })
      removeModal()
    })
    
    ##################
    # Create New Version and Request Access --------------------
    ##################
    ## modal to select coc and display associated versions --------
    select_coc_modal <- function(title) {
      showModal(
        modalDialog(
          title = title,
          selectizeInput(
            ns('coc_dropdown'),
            label = "Please choose a CoC:",
            choices = c("Choose one" = "", sort(cocs$coc_code))
          ),
          if(title == "Create ORR Version") 
            helpText(
              paste0(
                'You will become the Version Owner for this version of the ', coc_requested(), ' ORR, 
                with the sole ability to manage other user requests to collaborate on this version. 
                Would you like to continue?')
            )
          else
            NULL,
          br(),
          br(),
          h5(id = ns("existing_versions_title"), "Existing versions"),
          DT::DTOutput(ns("existing_versions")),
          footer = tagList(
            if(title == "Create ORR Version") actionButton(ns('choose_coc'), label="Continue", class='btn-primary') else NULL,
            actionButton(ns('send_request'), label='Send Request', disabled = FALSE, class="btn-warning"),
            modalButton(label='Cancel')
          ),
          size = "l"
        ),
        session = session
      )
      
      has_existing_versions <- fnrow(versions_and_requests_for_selected_coc()) > 0
      shinyjs::toggle(id = "existing_versions_title", condition = has_existing_versions)
      shinyjs::toggle(id = "send_request", condition = has_existing_versions)
      shinyjs::toggle(id = "existing_versions", condition = has_existing_versions)
    }
    
    ## Create New version modal ------------
    observeEvent(input$create_new_version, {select_coc_modal('Create ORR Version')})
    
    ## Request Access to a CoC Version -----------------
    observeEvent(input$request_access_direct, {select_coc_modal('Request Access to a CoC Version')})
    
    ## Import or Upload HIC ----------
    # User decides whether to import the HIC data as of X/X/XX date or upload their own
    observeEvent(input$choose_coc, {
      req(isTruthy(input$choose_coc))
     
      removeModal()
      
      choiceList <- setNames(
        c("import", "upload"), 
        c(
          paste0('Import the HIC data as of ', HDX_HIC_DATE),
          'Upload my own version of the HIC data'
        )
      )
      showModal(
        modalDialog(
          title = 'Inventory Data Source',
          radioButtons(
            ns('hic_import_select'),
            label = 'Which version of the HIC data would you like to use?',
            choices = choiceList,
            width = "100%"
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'upload'", ns('hic_import_select')),
            fileInput(ns('hic_file_upload'), label = 'Upload your HIC data', accept = c('csv'))
          ),
          footer = tagList(
            actionButton(inputId=ns('name_version'),label='Continue', class="btn-primary"),
            modalButton(label='Cancel')
          )
        ),
        session = session
      )
    })
    
    ## Name coc version -----------
    observeEvent(input$name_version, {
      
      initial_version_name <- paste0(input$coc_dropdown, '-', str_to_upper(user_coc$given_name))

      showModal(
        modalDialog(
          title = 'Name your CoC Version',
          textInput(
            ns("create_version_name"), 
            "Version Name",
            value = initial_version_name,
          ),
          footer = tagList(
            actionButton(ns('create_orr_confirm'), label="Create New Version", icon('circle-plus'), class="btn-primary"),
            modalButton(label="Cancel")
          ),
          easyClose = TRUE
        ),
        session = session
      )
      
    })
    
    ## Create new version from HIC ----------------
    create_new_version_for_user <- function(p, new_version_data) {
      new_version <- new_version_data |>
        fmutate(coc_status = get_lookup_refid("Not Started", "coc_status")) |>
        add_user_stamp(user_coc, is_new = TRUE)
      
      # Update CoC Version in db, and grab autonumbered coc_version_id
      new_coc_version_info <- insert_and_return(
        p,
        "coc_versions", 
        new_version %>% fselect(-coc_name), 
        c("coc_version_id", "date_updated")
      )
      
      new_version_user <- data.table(
        coc_version_id = unlist(new_coc_version_info)[["coc_version_id"]],
        username = user_coc$username,
        coc_version_role = as.character(get_lookup_refid("Owner","coc_version_role"))
      ) |>
        add_user_stamp(user_coc, is_new = TRUE)
      
      # Next, update CoC Version USers in db
      DBI::dbAppendTable(p, 'coc_version_users', new_version_user)
      
      # Generate initial set of selected thresholds and factors
      generate_data_for_new_coc_version(p, new_version_user$coc_version_id)
      
      return(new_version_user$coc_version_id)
    }
    
    ## Pull HIC Data for coc ------------
    es_project_type <- get_lookup_refid("ES", "project_type")
    get_hic_data <- function(coc, coc_version_id) {
      bed_field_mapping <- c(
        all_fam_beds = "beds_hh_w_children", 
        ch_fam_beds = "ch_beds_hh_w_children",
        vet_fam_beds = "veteran_beds_hh_w_children", 
        par_youth_beds = "youth_beds_hh_w_children",
        vet_ind_beds = "veteran_beds_hh_wo_children",
        single_youth_beds = "youth_beds_hh_wo_children"
      )

      coc_data <- get_db_tbl("all_hic_data") |>
        fsubset(hudnum == coc) 

      project_data <- coc_data %>% # %>% needed for gvr to work
        fmutate(
          mckinneyvento = factor_yesno(rowSums(gvr(., "mckinneyvento"), na.rm = TRUE) > 0),
          # mckinneyventoyhdp = factor_yesno(mckinneyventoyhdp),
          dv_renewal = factor_yesno(NA),
          grant_number = as.character(NA), 
          coc_amount_awarded_last_year = as.numeric(NA),
          coc_amount_expended_last_year = as.numeric(NA),
          coc_funding_requested = as.numeric(NA),
          funding_action = fifelse(
            project_type == es_project_type | mckinneyvento == "No",
            "Ignore", 
            "Renew"
          ),
          coc_version_id = coc_version_id,
          # additional cols user will fill out
          is_dedicated_ch_fam = factor_yesno(NA),
          is_dedicated_ch_ind = factor_yesno(NA),
          is_dedicated_dv = factor_yesno(NA),
          amount_other_public_funding = as.numeric(NA),
          amount_private_funding = as.numeric(NA),
          all_ind_beds = beds_hh_wo_children + beds_hh_w_only_children,
          total_ch_ind_beds = ch_beds_hh_wo_children + ch_beds_hh_w_only_children,
          dv_fam_beds = fifelse(target_population == "DV", beds_hh_w_children, as.integer(0)),
          dv_ind_beds = fifelse(target_population == "DV", all_ind_beds, as.integer(0))
        ) %>% # %>% needed for convert_to_factor to work
        fmutate(
          funding_action = convert_to_factor(., "funding_action", textToNum = T),
          project_type = convert_to_factor(., "project_type", textToNum = F),
          target_population = convert_to_factor(., "target_population", textToNum = F),
          created_by = SERVICE_ACCOUNT
        ) |>
        frename(bed_field_mapping) |>
        get_vars(setdiff(dbListFields(get_db_pool(), "projects"), "project_id"))

      return(project_data)
    }
    
    ## Confirm create ---------------
    observeEvent(input$create_orr_confirm, {
      req(input$hic_import_select == 'import')
      pool::poolWithTransaction(get_db_pool(), function(p) {
        coc_version_id <- create_new_version_for_user(
          p,
          data.table(
            coc_version_name = input$create_version_name,
            coc = input$coc_dropdown,
            coc_name = cocs$coc_name[cocs$coc_code == input$coc_dropdown]
          )
        )
        
        # Initialize projects data
        data <- get_hic_data(input$coc_dropdown, coc_version_id)
        
        filtered_data_db <- factor_vars_db_prep(data)
        if(IN_DEV_MODE && inherits(filtered_data_db$date_created, "POSIXct")) 
          filtered_data_db <- filtered_data_db |>
          fmutate(
            date_created = format(date_created, "%Y-%m-%d %H:%M:%S"),
            date_updated = format(date_updated, "%Y-%m-%d %H:%M:%S")
          )
        
        DBI::dbAppendTable(p, "projects", filtered_data_db)
      })
      
      shiny::showNotification('New CoC version created!', type='message')
      removeModal()
      refresh_trigger$versions <- refresh_trigger$versions + 1
    })
    
    #####################
    # Requesting Access to a CoC -------------------
    #####################
    # show the versions and user's request statuses in a datatable so user knows 
    # whether to request or not and can select a version to request
    versions_and_requests_for_selected_coc <- reactive({
      if(!isTruthy(input$coc_dropdown)) return(data.table())
      
      all_versions_and_users() |>
        fsubset(coc == input$coc_dropdown) |>
        fmutate(is_logged_in_user = username == user_coc$username) |>
        fgroup_by(coc_version_id) |>
        fmutate(owner = ffirst(fifelse(coc_version_role == "Owner", username, NA))) |>
        fungroup() |>
        roworder(coc_version_id, -is_logged_in_user) |>
        fslice(coc_version_id, n = 1) |>
        fselect(coc, coc_version_name, owner, is_logged_in_user, coc_version_id, coc_version_role) |>
        join(requests_by_user(), on = "coc_version_id") |>
        fmutate(
          request_status = get_lookup_label(request_status, "request_status"),
          request_status = fcase(
            is_logged_in_user, paste0("Already an ", coc_version_role),
            !is.na(request_status), paste0("Already ", request_status), 
            default = "Not yet requested"
          )
        ) |>
        fselect(-is_logged_in_user, -coc_version_role)
    })
    
    ## enable/disable request actionbuttons based on if a CoC version is selected from the modal table
    observe({
      shinyjs::toggleState(id = 'send_request', condition = length(input$existing_versions_rows_selected) > 0)
    })
    
    # Datatable of versions and user's requests 
    # shown in Create ORR and Request Access to CoC modals
    output$existing_versions <- renderDT({
      req(input$coc_dropdown)
      
      versions_to_show <- versions_and_requests_for_selected_coc() |>
        fselect(-coc_version_id)
      
      req(fnrow(versions_to_show) > 0)
      
      datatable(
        versions_to_show,
        colnames = c("CoC", "Version Name", "Owner", "Availability"),
        rownames = FALSE,
        style = 'default',
        selection = 'multiple',
        options = list(
          dom = 'ft',
          # disable rows that have already been requested
          rowCallback = JS(
            sprintf("function(row, data, index) {
              var status = data[%s];
              if (status !== null && status != 'Not yet requested') {
                $(row).css({
                  'pointer-events': 'none',
                  'opacity': '0.5',
                  'background-color': '#ebebeb',
                });
              }
            }", which(names(versions_to_show) == "request_status") - 1)
          )
        )
      )
    })
    existing_versions_proxy <- dataTableProxy(ns("existing_versions"), session = session)
    observeEvent(versions_and_requests_for_selected_coc(), {
      has_existing_versions <- fnrow(versions_and_requests_for_selected_coc()) > 0
      
      shinyjs::toggle(id = "existing_versions_title", condition = has_existing_versions)
      shinyjs::toggle(id = "send_request", condition = has_existing_versions)
      shinyjs::toggle(id = "existing_versions", condition = has_existing_versions)
      
      replaceData(
        existing_versions_proxy, 
        versions_and_requests_for_selected_coc() |> fmutate(coc_version_id = NULL), 
        rownames = FALSE
      )
    })
    
    
    # Create Version Request in DB, pass success to requests module, notify user
    observeEvent(input$send_request, {
      selected_version <- versions_and_requests_for_selected_coc()[input$existing_versions_rows_selected]
      s <- append_version_request(selected_version, user_coc)
      if(isTruthy(s))
        refresh_trigger$request_sent <- refresh_trigger$request_sent + 1
      
      ## TODO: send email to admin of version that is requested
      
      user_coc$requests_updated <- user_coc$requests_updated + 1
      
      removeModal()
      
      showNotification('Request sent!', duration = 3)
    })
  })
}
