mod_coc_selection_ui <- function(id) {
  ns <- NS(id)

  card(id = id,
    #card_header(h4("Versions"))
    card_header(h4("Versions", 
                   div(style = "float: right;",
                   actionBttn(ns('refresh_versions_tbl'), label="Refresh", color="primary", size="xs", icon=icon('refresh')))
                )),
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
    
    ## subset coc_version_users to specific user
    coc_vu <- reactiveVal(NULL)
    
    ## session variables used for sending access requests
    admin_email <- reactiveVal(NULL)
    coc_requested <- reactiveVal(NULL)
    version_requested <- reactiveVal(NULL)
    
    get_all_users_and_versions <- function() {
      coc_vu(
        get_coc_versions_for_user(user_coc$username) |>
          fselect(-created_by) |>
          fmutate(
            coc_version_role = get_lookup_label(coc_version_role, 'coc_version_role'),
            coc_status = get_lookup_label(coc_status, 'coc_status')
          ) |>
          join(
            cocs %>% fselect(coc_code, coc_name),
            how = 'left', 
            on = c('coc' = 'coc_code')
          ) |>
          colorder(coc, coc_name, pos = "after")
      )
    }
    
    observeEvent(user_coc$auth, {
      req(user_coc$auth)
      get_all_users_and_versions()
    })
    
    observeEvent(input$refresh_versions_tbl, {
      showNotification("Refreshing Versions table!", type = "message")
      get_all_users_and_versions()
    })
    
    project_ids <- reactive({
      req(user_coc$coc_version_id)

      get_db_query(
        "SELECT project_id FROM projects WHERE coc_version_id = $1", 
        params = user_coc$coc_version_id
      )$project_id
    })
    
    owner_role_refid <- get_lookup_refid("Owner", "coc_version_role")
    
    ####
    # CoC Versions table ------------------
    ####
    coc_proxy <- dataTableProxy(ns('coc_versions_dt'))
    
    observe({
      req(coc_vu())
      replaceData(coc_proxy, coc_vu())
    })
    
    output$coc_versions_dt <- renderDT({
      req(user_coc$auth)
      
      datatable(coc_vu(), 
                colnames = unname(versions_variable_labels[match(names(coc_vu()),  names(versions_variable_labels))]),
                rownames = FALSE,
                options = list(
                  dom = 'tip', 
                  autowidth = FALSE,
                  columnDefs = list(
                    list(targets=0, className = "hidden")
                  )
                ),
                editable = FALSE,
                style = 'default',
                #filter = list(position = 'top', plain = TRUE),
                selection = 'single'
      ) %>% 
        formatDate(
          columns = c('date_created', 'date_updated'),
          method = 'toLocaleString'
        )
    })
    
    ####
    # CoC Version Actions --------------
    ####
    
    ## Enable/disable actions when row is selected or not
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
    observe({
      req(user_coc$auth)
      
      # toggle Inventory tab if they have any versions selected
      toggle_navs_on_coc_selection()
      
      shinyjs::toggle(id = 'edit_coc_version', condition = length(input$coc_versions_dt_rows_selected) > 0)
      shinyjs::toggle(id = 'delete_coc_version', condition = length(input$coc_versions_dt_rows_selected) > 0)
      shinyjs::toggle(id = 'copy_version', condition = length(input$coc_versions_dt_rows_selected) > 0)

      # If there are any versions NOT associated with the current user, allow them to Request Access
      if(nrow(coc_vu()) > 0) {
        shinyjs::toggle(id = 'request_access_direct', condition = coc_vu() |> 
                          fgroup_by(coc) |> 
                          fsummarize(no_version_access = !any(username == user_coc$username)) |> 
                          fsubset(no_version_access) |> 
                          nrow() > 0
        )
      }
    })
    
    ## Edit version ----------------
    observeEvent(input$coc_versions_dt_rows_selected, {
      current_coc_info <- coc_vu()[input$coc_versions_dt_rows_selected, .(coc, coc_version_id)]
      user_coc$coc <- current_coc_info$coc
      user_coc$coc_version_id <- current_coc_info$coc_version_id
      user_coc$date_updated <- current_coc_info$date_updated
    })
    observeEvent(input$edit_coc_version, {
      req(user_coc$auth)
      ## update versions table to "In progress" when editing begins
      db_execute( 
        "UPDATE coc_versions SET coc_status = $1, 
        date_updated = CURRENT_TIMESTAMP, updated_by = $2
        WHERE coc_version_id = $3 AND date_updated = $4", 
        params = list(7, user_coc$username, user_coc$coc_version_id, user_coc$date_updated)
      )
      nav_control("inventory")
    })
    
    ## Delete version ---------------
    observeEvent(input$delete_coc_version, {
      
      if(coc_vu()[input$coc_versions_dt_rows_selected, coc_version_role] == "Owner"){
        
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
    
    
    ## Create new version --------------
    #  When they hit Create: display pop-up form titled "Create ORR" with a simple dropdown to select a CoC.
    observeEvent(input$create_new_version, {
      shiny::invalidateLater(100)
      showModal(
        modalDialog(
          title = 'Create ORR',
          selectizeInput(
            ns('coc_dropdown'),
            label = "Please choose a CoC:",
            choices = sort(cocs$coc_code)
          ),
          footer = tagList(
            actionButton(ns('choose_coc'), label="Continue", class='btn-primary'),
            modalButton(label="Cancel")
          ),
          easyClose = TRUE
        ),
        session = session
      )
    })
    
    ## Copy version ------------
    observeEvent(input$copy_version, {
      req(input$coc_versions_dt_rows_selected)
      current_version_name <- coc_vu()[input$coc_versions_dt_rows_selected, .(coc_version_name)]
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
    
    create_new_version_for_user <- function(new_version_data) {
      new_version <- new_version_data |>
        fmutate(coc_status = get_lookup_refid("Not Started", "coc_status")) |>
        add_user_stamp(user_coc, is_new = TRUE)
      
      # Update CoC Version in db, and grab autonumbered coc_version_id
      new_coc_version_info <- insert_and_return(
        "coc_versions", new_version %>% fselect(-coc_name), c("coc_version_id", "date_updated")
      )

      new_version_user <- data.table(
        coc_version_id = unlist(new_coc_version_info)[["coc_version_id"]],
        username = user_coc$username,
        coc_version_role = as.character(get_lookup_refid("Owner","coc_version_role"))
      ) |>
        add_user_stamp(user_coc, is_new = TRUE)
      
      # Next, update CoC Version USers in db
      db_append('coc_version_users', new_version_user)
      
      # update reactiveVal
      coc_vu(
        rbind(
          copy(coc_vu()), 
          new_version |>
            fmutate(
              coc_version_id = new_version_user$coc_version_id,
              coc_version_role = new_version_user$coc_version_role,
              coc_status = get_lookup_label(coc_status, "coc_status"),
              coc_version_role = get_lookup_label(coc_version_role, "coc_version_role"),
              date_updated = as.POSIXct(new_coc_version_info[[1]]$date_updated),
              date_created = as.POSIXct(new_coc_version_info[[1]]$date_updated)
            ),
          fill=TRUE
        ) %>% fselect(-created_by)
      )
      
      return(new_version_user$coc_version_id)
    }
    
    observeEvent(input$copy_orr_confirm, {
      coc_version_id <- create_new_version_for_user(
        coc_vu()[input$coc_versions_dt_rows_selected] |>
          fmutate(coc_version_name = input$copy_version_name) |>
          fselect(-coc_version_role, -date_updated, -date_created)
      )
      removeModal()
      # TODO: Eventually build out copying all the otehr tables
      # copy_additional_data()
    })
    
    ####
    # Importing/Uploading HIC Data ------------------
    ####
    output$hic_cond_select <- renderUI({
      req(input$hic_import_select)
      
      # If they choose to upload: display a file upload input and allow them to select a csv
      if(input$hic_import_select == 'upload'){
        fileInput(ns('hic_file_upload'), label = 'Upload your HIC data',
                  accept = c('csv'))
      } else if(input$hic_import_select == 'import'){
        
      }
    })
    # Once they select a CoC, close the previous modal and show another one with a radio button asking if they want to import the HIC data or import the HIC data as of X/X/XX date.
    observeEvent(input$choose_coc, {
      
      
      coc_requested(input$coc_dropdown)
      
      # If there’s an version THEY are already associated with (by looking up CoC Versions joined with CoC Version Users where the user is this user),
      # warn them that they already have an ORR for this CoC and that if they wish to modify settings, they can do so within existing ORRs.
      # Show options "Continue" or "Cancel"
      check_if_already_have <- coc_vu() |>
        fsubset(username == user_coc$username & coc == coc_requested())
      
      check_if_others_have <- coc_vu() |>
        fsubset(username != user_coc$username & coc == coc_requested() & coc_version_role == owner_role_refid)
      
      removeModal()
      
      if(nrow(check_if_already_have) > 0){
        admin_email(NULL)
        version_requested(NULL)
        #version_requested(check_if_already_have$coc_version_id)
        showModal(modalDialog(
          title = 'Your own CoC Version Found',
          helpText(paste0('You have ', nrow(check_if_already_have), ' existing versions for CoC: ', coc_requested(),'. If you wish to modify settings, 
                          you can do so within the existing ORR and click "Cancel". If you still wish to create a new version, please click "Continue."')),
          footer = tagList(
            # If they continue: go to next step
            actionButton(ns('continue_new_version'), label='Continue', class="btn-primary"),
            # If they cancel: close pop-up
            modalButton(label='Cancel')
          )
        ))
        
        # If they select a CoC that SOMEONE ELSE is associated with (by looking up CoC Versions joined with CoC Version Users where the user is NOT this user and user role = "Admin"), 
      } else if(nrow(check_if_others_have) > 0){
        admin_email(check_if_others_have$username)
        version_requested(check_if_others_have$coc_version_id)
        # let them know as much and provide them the option to "Request Access" or "Create ORR anyway"
        showModal(modalDialog(
          title = 'Other owned CoC Version Found',
          helpText(paste0('Another user (', check_if_others_have$username ,') has an existing version for CoC: ', coc_requested(),'. Would you like to request
                          access from this user, or continue creating a new version for this CoC?')),
          footer = tagList(
            actionButton(ns('request_access_indirect'), label='Request Access', icon = icon('unlock'), class="btn-warning"),
            # If they continue: go to next step
            actionButton(ns('continue_new_version2'), label='Create New Version', icon('circle-plus'), class="btn-primary"),
            modalButton('Cancel')
          )
        ))
       
      } else {
        admin_email(NULL)
        version_requested(NULL)
        # If they select a CoC that has no CoC Versions: go to next step
        showModal(modalDialog(
          title = paste0('Create ORR for ', coc_requested()),
          helpText(paste0('You will become the Version Owner for this version of the ', coc_requested(), ' ORR, with the sole ability to manage other user requests to collaborate on this version. Would you like to continue?')),
          footer = tagList(
            # If they continue: go to next step
            actionButton(ns('continue_new_version3'), label='Continue', class="btn-primary"),
            # If they cancel: close pop-up
            modalButton(label='Cancel')
          )
        ))
      }
   
    })
    
    observeEvent(
      c(input$continue_new_version, input$continue_new_version2, input$continue_new_version3),
      {
        req(isTruthy(input$continue_new_version) || isTruthy(input$continue_new_version2) ||
            isTruthy(input$continue_new_version3))
       
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
            uiOutput(ns('hic_cond_select')),
            footer = tagList(
              actionButton(inputId=ns('new_hic_version'),label='Continue', class="btn-primary"),
              modalButton(label='Cancel')
            )
          ),
          session = session
        )
      }
    )
    
    
    # Requesting access to a CoC directly ---------------
    # When user clicks the "Request Access to a CoC" button on the dashboard
    # allow user to view versions and request access
    request_access_direct_coc_versions <- reactive({
      coc_vu() |>
        fsubset(username != user_coc$username) |>
        fselect(coc, coc_version_name, username)
    })
    # When user clicks the "Request Access to a CoC" button
    observeEvent(input$request_access_direct, {

      showModal(modalDialog(
        title = 'Request Access to a CoC',
        selectizeInput(
          ns('request_access_coc_dropdown'),
          label = "Please choose a CoC to view its versions:",
          choices = sort(funique(request_access_direct_coc_versions()$coc))
        ),
        DT::DTOutput(ns("direct_request_coc_versions")),
        footer = tagList(
          # If they continue: go to next step
          actionButton(ns('send_direct_request'), label='Send Request', disabled = FALSE, class="btn-warning"),
          # If they cancel: close pop-up
          modalButton(label='Cancel')
        ),
        size = "l"
      ))
    })

    output$direct_request_coc_versions <- renderDT({
      req(input$request_access_coc_dropdown)
      
      versions_to_show <- request_access_direct_coc_versions() |>
        fsubset(coc == input$request_access_coc_dropdown & 
                username != user_coc$username)
      
      req(nrow(versions_to_show) > 0)
      
      datatable(
        versions_to_show,
        colnames = c("CoC", "Version Name", "Owner"),
        rownames = FALSE,
        options = list(dom = 'tip'),
        style = 'default',
        selection = 'multiple'
      )
    })
    
    create_request <- function(cur_coc, version_id) {
      request_status_num <- get_lookup_refid('Sent','request_status')
      
      request_row <- data.table(
        #coc_request_id = 1 + (get_db_tbl('coc_version_requests') |> fnrow()),
        coc_version_id = version_id,
        request_status = request_status_num,
        reason_for_rejection = NA
      ) |>
        add_datetime_stamp(is_new = TRUE) |>
        add_user_stamp(user_coc, is_new = TRUE)
      
      # Add row to requests table
      db_append("coc_version_requests", request_row)
    }
    
    observeEvent(input$send_direct_request, {
      
      prev_requests <- get_db_tbl('coc_version_requests')
      
      version_id <- coc_vu() |> 
        fsubset(coc == input$request_access_coc_dropdown & 
                coc_version_name == input$direct_request_coc_versions_cell_clicked$value) |> 
        fselect('coc_version_id') %>% 
        ffirst()
      
      check_if_already_requested <- prev_requests %>% 
        fsubset(coc_version_id == version_id & 
                  created_by == user_coc$username)
      
      if(fnrow(check_if_already_requested) > 0){
        showNotification('You already have an outstanding request for this CoC Version. Please select another one.')
      } else {
        # TODO: Send email to version Owners of input$direct_request_coc_versions_rows_selected
        create_request(cur_coc = input$request_access_coc_dropdown,
                       version_id = version_id)
        removeModal()
        showNotification('Request sent!', duration = 3)
      }
     
    })
    
    # Requesting access to a CoC indirectly ---------------
    # This is for when the user tried to create a new version 
    # but may not have known about an existing version for the same CoC
    request_access_indirect_coc_versions <- reactive({
      
      coc_vu() |>
        fsubset(username != user_coc$username) |>
        fselect(coc, coc_version_name, username)
    })
    
    # When user clicks the "Request Access" button within the "Create Version" flow
    observeEvent(input$request_access_indirect, {
      
      ## TODO: Allow user to select a CoC Version and request access indirectly
      showModal(modalDialog(
        title = 'Request Access to a CoC',
        helpText('Select a CoC to view its versions...'),
        selectInput(ns('request_indirect_access_coc_dropdown'),
                    label = "Please choose a CoC:",
                    choices = sort(funique(request_access_indirect_coc_versions()$coc))
        ),
        DT::DTOutput(ns("indirect_request_coc_versions")),
        footer = tagList(
          # If they continue: go to next step
          actionButton(ns('send_indirect_request'), label='Send Request', disabled = FALSE, class="btn-warning"),
          # If they cancel: close pop-up
          modalButton(label='Cancel')
        )
      ))
    })
    
    ## enable/disable request actionbuttons based on if a CoC version is selected from the modal table
    observe({
      shinyjs::toggleState(id = 'send_direct_request', condition = length(input$direct_request_coc_versions_rows_selected) > 0)
      
      shinyjs::toggleState(id = 'send_indirect_request', condition = length(input$indirect_request_coc_versions_rows_selected) > 0)
    })
    
    output$indirect_request_coc_versions <- renderDT({
      req(input$request_indirect_access_coc_dropdown)
      
      versions_to_show <- request_access_indirect_coc_versions() |>
        fsubset(coc == input$request_indirect_access_coc_dropdown & 
                  username != user_coc$username)
      
      req(nrow(versions_to_show) > 0)
      
      datatable(
        versions_to_show,
        colnames = c("CoC", "Version Name", "Owner"),
        rownames = FALSE,
        options = list(dom = 'tip'),
        style = 'default',
        selection = 'multiple'
      )
    })
    
    # If they "Request Access" after trying to create a new CoC Version but one 
    # is already created: 
    # send email to user associated with that other CoC Version
    observeEvent(input$send_indirect_request, {
      #req(!is.null(admin_email()))
      
      
      prev_requests <- get_db_tbl('coc_version_requests')
      
      version_id <- coc_vu() |> 
        fsubset(coc == input$request_indirect_access_coc_dropdown & 
                  coc_version_name == input$indirect_request_coc_versions_cell_clicked$value) |> 
        fselect('coc_version_id') %>% 
        ffirst()
      
      check_if_already_requested <- prev_requests %>% 
        fsubset(coc_version_id == version_id & 
                  created_by == user_coc$username)
      
      if(fnrow(check_if_already_requested) > 0){
        showNotification('You already have an outstanding request for this CoC Version. Please select another one.')
      } else {
        ## TODO: send email to admin of version that is requested
        
        create_request(cur_coc = input$request_indirect_access_coc_dropdown,
                       version_id = version_id)
        removeModal()
        showNotification('Request sent!', duration = 3)
      }
      
    })
    
    observeEvent(input$new_hic_version, {
      req(input$hic_import_select == 'import')
      
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
    
    # Creating a new ORR from the HIC ----------------
    observeEvent(input$create_orr_confirm, {
      req(input$hic_import_select == 'import')
      
      coc_version_id <- create_new_version_for_user(
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
      
      db_append("projects", filtered_data_db)
      
      shiny::showNotification('New CoC version created!', type='message')
      removeModal()
      
    })
    
    
    
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
          mckinneyventoyhdp = factor_yesno(mckinneyventoyhdp),
          dv_renewal = factor_yesno(NA),
          grant_number = as.character(NA), 
          coc_amount_awarded_last_year = as.numeric(NA),
          coc_amount_expended_last_year = as.numeric(NA),
          coc_funding_requested = as.numeric(NA),
          funding_action = fifelse(mckinneyvento == "Yes", "Renew", "Ignore"),
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
        get_vars(setdiff(dbListFields(DB_POOL, "projects"), "project_id"))

      return(project_data)
    }

  })
}
