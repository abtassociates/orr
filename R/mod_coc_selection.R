mod_coc_selection_ui <- function(id) {
  ns <- NS(id)
  
  #nav_panel(
  #  "My Dash",
  #  value = id,
    card(id = id,
      card_header(h4("Versions")),
      card_body(
        fillable = FALSE,
        p('A CoC can have multiple versions of its ORR. Versions can be created to play around or test different combinations of factors and parameters. Multiple users can collaborate on a single or multiple versions.'),
        p('To collaborate on an existing version, click "Request Access to a CoC". To create your own version, click "Create New Version". To create a copy of an existing version, click "Copy Version".'),
        # a "Create" button or link above the table will display so they can create a new CoC Version
        DTOutput(ns('coc_versions_dt'),fill = F) |> shinycssloaders::withSpinner(),
        actionButton(ns('create_new_version'), "Create New Version", icon = icon('circle-plus'), class='btn-primary'),
        actionButton(ns('edit_coc_version'),"Edit Selected Version", icon = icon('edit'), class='btn-secondary'),
        actionButton(ns('delete_coc_version'), "Delete Selected Version", icon = icon('trash'), class='btn-danger'),
        actionButton(ns('copy_version'), "Copy Version", icon = icon('copy'), class="btn-info"),
        actionButton(ns('request_access_direct'), "Request Access to a CoC", icon = icon('unlock'), class="btn-warning")
      )
    )
  #)
}

mod_coc_selection_server <- function(id, nav_control, projects_data, user_coc) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    ## subset coc_version_users to specific user
    coc_vu <- reactiveVal(NULL)
    
    ## session variables used for sending access requests
    admin_email <- reactiveVal(NULL)
    coc_requested <- reactiveVal(NULL)
    version_requested <- reactiveVal(NULL)
    
    owner_role_refid <- get_lookup_refid("Owner", "coc_version_role")
    
    ####
    # CoC Versions table ------------------
    ####
    observe({
      req(user_coc$auth)
      coc_vu(
        coc_version_users |>
          fsubset(username == user_coc$email, -c(username, date_created, created_by, coc_version_id)) |>
          fmutate(
            coc_version_role = get_lookup_label(coc_version_role, 'coc_version_role'),
            coc_status = get_lookup_label(coc_status, 'coc_status')
          )
      )
    })
    
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
                options = list(dom = 'tip'),
                editable = FALSE,
                style = 'default',
                #filter = list(position = 'top', plain = TRUE),
                selection = 'single'
      )
    })
    
    ####
    # CoC Version Actions --------------
    ####
    
    ## Enable/disable actions when row is selected or not
    observe({
      req(user_coc$auth)
      shinyjs::toggle(id = 'edit_coc_version', condition = length(input$coc_versions_dt_rows_selected) > 0)
      shinyjs::toggle(id = 'delete_coc_version', condition = length(input$coc_versions_dt_rows_selected) > 0)
      shinyjs::toggle(id = 'copy_version', condition = length(input$coc_versions_dt_rows_selected) > 0)
      # If there are any versions NOT associated with the current user, allow them to Request Access
      if(nrow(coc_version_users) > 0) {
        shinyjs::toggle(id = 'request_access_direct', condition = coc_version_users |> 
                          fgroup_by(coc) |> 
                          fsummarize(no_version_access = !any(username == user_coc$email)) |> 
                          fsubset(no_version_access) |> 
                          nrow() > 0
        )
      }
    })
    
    ## Edit version ----------------
    observeEvent(input$edit_coc_version, {
      # Save the selected CoC and CoC Version in user_coc, to be passed around to other modules
      current_coc_info <- coc_vu()[input$coc_versions_dt_rows_selected, .(coc, coc_version_id)]

      user_coc$coc <- current_coc_info$coc
      user_coc$coc_version_id <- current_coc_info$coc_version_id

      
      # Store the project data in the projects_data to be passed to other modules
      filtered_data <- get_db_tbl("projects") |>
        fsubset(coc_version_id == user_coc$coc_version_id)
      
      projects_data(filtered_data)

      nav_control("inventory")
    })
    
    ## Delete version ---------------
    observeEvent(input$delete_coc_version, {
      showModal(
        modalDialog(
          title = 'Confirm Deletion',
          helpText("Are you sure you want to delete this CoC version? This action cannot be undone."),
          footer = tagList(
            actionButton(ns('confirm_deletion'), label='Confirm'),
            modalButton(label='Cancel')
          )
        )
      )
    })
    
    
    ## Create new version --------------
    #  When they hit Create: display pop-up form titled "Create ORR" with a simple dropdown to select a CoC.
    observeEvent(input$create_new_version, {
      showModal(
        modalDialog(
          title = 'Create ORR',
          selectInput(ns('coc_dropdown'),
                      label = "Please choose a CoC:",
                      choices = sort(cocs$coc_code),
                      ),
          footer = tagList(
            actionButton(ns('choose_coc'), label="Next"),
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
      
      showModal(
        modalDialog(
          title = 'Copy ORR',
          textInput(
            ns("copy_version_name"), 
            "Version Name",
            placeholder = paste0(
              coc_vu()[input$coc_versions_dt_rows_selected, .(coc_version_name)],
              "_v2"
            )
          ),
          footer = tagList(
            actionButton(ns('copy_orr_confirm'), label="Confirm"),
            modalButton(label="Cancel")
          ),
          easyClose = TRUE
        ),
        session = session
      )
    })
    
    create_new_version_for_user <- function(new_version_data) {
      new_version <- new_version_data |>
        add_user_stamp(user_coc, is_new = TRUE)
      
      # Update CoC Version in db, and grab autonumbered coc_version_id
      new_coc_version_id <- insert_and_return(
        "coc_versions", new_version, "coc_version_id"
      )
      
      new_version_user <- data.table(
        coc_version_id = unname(unlist(new_coc_version_id)),
        username = user_coc$email,
        coc_version_role = as.character(get_lookup_refid("Owner","coc_version_role")),
        new_version %>% fselect(created_by, date_updated, updated_by, coc)
      )
      
      # Next, update CoC Version USers in db
      dbAppendTable(DB_CON, 'coc_version_users', new_version_user %>% fselect(-coc))
      
      # update reactiveVal
      coc_vu(
        rbind(
          copy(coc_vu()), 
          new_version |>
            fmutate(coc_version_role = new_version_user$coc_version_role),
          fill=TRUE
        )
      )
    }
    
    observeEvent(input$copy_orr_confirm, {
      create_new_version_for_user(
        coc_vu()[input$coc_versions_dt_rows_selected] |>
          fmutate(coc_version_name = input$copy_version_name)
      )
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
      check_if_already_have <- coc_version_users |>
        fsubset(username == user_coc$username & coc == coc_requested())
      
      check_if_others_have <- coc_version_users |>
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
            actionButton(ns('continue_new_version'), label='Continue'),
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
            actionButton(ns('request_access_indirect'), label='Request Access'),
            # If they continue: go to next step
            actionButton(ns('continue_new_version2'), label='Create ORR anyway')
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
            actionButton(ns('continue_new_version3'), label='Continue'),
            # If they cancel: close pop-up
            modalButton(label='Cancel')
          )
        ))
      }
   
    })
    
    observeEvent(
      c(input$continue_new_version, input$continue_new_version2, input$continue_new_version3),
      {
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
              choices = choiceList
            ),
            uiOutput(ns('hic_cond_select')),
            footer = tagList(
              actionButton(inputId=ns('new_hic_version'),label='Create New Version'),
              modalButton(label='Cancel')
            )
          ),
          session = session
        )
      }, ignoreInit = TRUE
    )
    
    # If they "Request Access" after trying to create a new CoC Version but one 
    # is already created: 
    # send email to user associated with that other CoC Version
    observeEvent(input$request_access_indirect, {
      req(!is.null(admin_email()))
      
      ## TODO: send email to admin of version that is requested
      
      
      showModal(
        modalDialog(
          title = 'Request Sent',
          helpText('Thank you. A request has been sent to the Admin for this version.'),
          HTML(paste0('<p>Request Details</p>
               <ul>
               <li>Requested CoC: ', input$coc_dropdown,'</li>',
               '<li>Requested version: ','</li>',    
               '<li>Requested at: ',Sys.time(),'</li>',
               '</ul>
               '))
        )
      )
    })
    
    
    # Requesting access to a CoC directly ---------------
    # allow user to view versions and request access
    request_access_direct_coc_versions <- reactive({
      req(input$request_access_coc_dropdown)

      coc_version_users |>
        fsubset(
          username != user_coc$email & 
            coc == input$request_access_coc_dropdown &
            coc_version_role == owner_role_refid,
          coc, coc_version_name, username
        )
    })
    # When user clicks the "Request Access to a CoC" button
    observeEvent(input$request_access_direct, {

      ## TODO: Allow user to select a CoC Version and request access directly
      showModal(modalDialog(
        title = 'Request Access to a CoC',
        helpText('Select a CoC to view its versions...'),
        selectInput(ns('request_access_coc_dropdown'),
                    label = "Please choose a CoC:",
                    choices = sort(funique(coc_version_users$coc))
        ),
        DT::DTOutput(ns("direct_request_coc_versions")),
        footer = tagList(
          # If they continue: go to next step
          actionButton(ns('send_direct_request'), label='Send Request', disabled = TRUE),
          # If they cancel: close pop-up
          modalButton(label='Cancel')
        )
      ))
    })

    observeEvent(
      input$direct_request_coc_versions_rows_selected,
      updateActionButton(
        ns('send_direct_request'), 
        disabled = length(input$direct_request_coc_versions_rows_selected) > 0
      )             
    )
    output$direct_request_coc_versions <- renderDT({
      req(input$request_access_coc_dropdown)
      datatable(
        request_access_direct_coc_versions(),
        colnames = c("CoC", "Version Name", "Owner"),
        rownames = FALSE,
        options = list(dom = 'tip'),
        selection = 'multiple'
      )
    })
    
    observeEvent(input$send_direct_request, {
      # TODO: Send email to version Owners of input$direct_request_coc_versions_rows_selected
      
    })
    
    
    # Creating a new ORR from the HIC ----------------
    observeEvent(input$new_hic_version, {
      req(input$hic_import_select == 'import')
      
      create_new_version_for_user(
        data.table(
          coc_version_name = paste0(input$coc_dropdown, '-', str_to_upper(user_coc$given_name)),
          coc = input$coc_dropdown, 
          coc_status = get_lookup_refid("Not Started", "coc_status")
        )
      )
      
      # Initialize projects data
      filtered_data <- get_hic_data(input$coc_dropdown, new_version_user$coc_version_id)
      projects_data(filtered_data)
      
      filtered_data_db <- factor_vars_db_prep(filtered_data)
      DBI::dbAppendTable(DB_CON, "projects", filtered_data_db)
      
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

      project_data <- coc_data %>%
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
        ) %>%
        fmutate(
          funding_action = convert_to_factor(., "funding_action", textToNum = TRUE),
          project_type = convert_to_factor(., "project_type", textToNum = TRUE),
          target_population = convert_to_factor(., "target_population", textToNum = TRUE),
          created_by = SERVICE_ACCOUNT
        ) %>%
        frename(bed_field_mapping) %>%
        get_vars(dbListFields(DB_CON, "projects"))

      return(project_data)
    }

  })
}
