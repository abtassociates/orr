mod_coc_selection_ui <- function(id) {
  ns <- NS(id)
  
  #nav_panel(
  #  "My Dash",
  #  value = id,
    card(id = id,
      card_header(h4("Instances")),
      card_body(
        fillable = FALSE,
        p('Instances are versions of a CoC\'s ORR. Multiple instances can be created to play around or test different combinations of factors and parameters. Multiple users can collaborate on a single or multiple instances.'),
        p('To collaborate on an existing instance, click "Request Access". To create your own instance, click "Create New Instance". To create a copy of an existing instance, click "Copy Instance".'),
        # a "Create" button or link above the table will display so they can create a new CoC Instance
        DTOutput(ns('coc_instances_dt'),fill = F) |> shinycssloaders::withSpinner(),
        actionButton(ns('edit_coc_instance'),"Edit Selected Instance", icon = icon('edit'), class='btn-primary'),
        actionButton(ns('delete_coc_instance'), "Delete Selected Instance", icon = icon('trash'), class='btn-danger'),
        actionButton(ns('create_new_instance'), "Create New Instance", icon = icon('circle-plus'), class='btn-secondary'),
        
      )
    )
  #)
}

mod_coc_selection_server <- function(id, nav_control, projects_data, user_coc) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    ## subset coc_instance_users to specific user
    coc_iu <- reactive({
      req(user_coc$auth)
      
      get_db_tbl('coc_instance_users') |>
        fselect(1:(length(dbListFields(DB_CON,name = 'coc_instance_users')) -1)) |>
        fsubset(username == user_coc$email) |>
        fmutate(coc_instance_role = get_lookup_label(coc_instance_role, 'coc_instance_role'))
    })
    
    ## session variables used for sending access requests
    admin_email <- reactiveVal(NULL)
    coc_requested <- reactiveVal(NULL)
    instance_requested <- reactiveVal(NULL)
    
    ## disable Edit button unless row is selected
    observe({
      if(length(input$coc_instances_dt_rows_selected)==0){
        shinyjs::disable(id = 'edit_coc_instance')
        shinyjs::disable(id = 'delete_coc_instance')
      } else {
        shinyjs::enable(id = 'edit_coc_instance')
        shinyjs::enable(id = 'delete_coc_instance')
      }
    })
    
    coc_proxy <- dataTableProxy(ns('coc_instances_dt'))
    
    observe({
      req(coc_iu())
      replaceData(coc_proxy, coc_iu())
    })
    
    output$coc_instances_dt <- renderDT({
      
      datatable(coc_iu(), 
                colnames = str_to_title(
                  str_replace_all(names(coc_iu()),'_',' ')
                ),
                rownames = FALSE,
                options = list(dom = 'tip'),
                editable = FALSE,
                style = 'default',
                #filter = list(position = 'top', plain = TRUE),
                selection = 'single'
      )
    })
    
    observeEvent(input$edit_coc_instance, {
      user_coc$coc <- coc_iu()$coc[[1]]
      print(user_coc$coc)
      
      user_coc$coc_instance_id <- coc_iu()[
        input$coc_instances_dt_rows_selected, .(coc_instance_id)
      ][[1]]
      
      nav_control("inventory")
      
      # Initialize projects data
      # If there are already Project records for this CoC Instance, store those. 
      # Otherwise, store the HIC data
      filtered_data <- get_db_tbl("projects")

      if(nrow(filtered_data) > 0) projects_data(filtered_data)
      else {
        filtered_data <- get_hic_data()
        projects_data(filtered_data)
        
        filtered_data_db <- factor_vars_db_prep(filtered_data)
        DBI::dbAppendTable(DB_CON, "projects", filtered_data_db)
      }
    })
    
    observeEvent(input$delete_coc_instance, {
      showModal(
        modalDialog(
          title = 'Confirm Deletion',
          helpText("Are you sure you want to delete this CoC instance? This action cannot be undone."),
          footer = tagList(
            actionButton(ns('confirm_deletion'), label='Confirm'),
            modalButton(label='Cancel')
          )
        )
      )
    })
    
    
    #  When they hit Create: display pop-up form titled "Create ORR" with a simple dropdown to select a CoC.
    observeEvent(input$create_new_instance, {
      showModal(
        modalDialog(
          title = 'Create ORR',
          selectInput(ns('coc_dropdown'),
                      label = "Please choose a CoC:",
                      choices = cocs$coc_code,
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
      
      # If there’s an instance THEY are already associated with (by looking up CoC Instances joined with CoC Instance Users where the user is this user),
      # warn them that they already have an ORR for this CoC and that if they wish to modify settings, they can do so within existing ORRs.
      # Show options "Continue" or "Cancel"
      check_if_already_have <- get_db_tbl('coc_instance_users') |> 
        fsubset(username == user_coc$username & coc == coc_requested())
      
      check_if_others_have <- get_db_tbl('coc_instance_users') |>
        fsubset(username != user_coc$username & coc == coc_requested() & coc_instance_role == 5)
      
      removeModal()
      
      
      if(nrow(check_if_already_have) > 0){
        admin_email(NULL)
        instance_requested(NULL)
        #instance_requested(check_if_already_have$coc_instance_id)
        showModal(modalDialog(
          title = 'Your own CoC Instance Found',
          helpText(paste0('You have ', nrow(check_if_already_have), ' existing instances for CoC: ', coc_requested(),'. If you wish to modify settings, 
                          you can do so within the existing ORR and click "Cancel". If you still wish to create a new instance, please click "Continue."')),
          footer = tagList(
            # If they continue: go to next step
            actionButton(ns('continue_new_instance'), label='Continue'),
            # If they cancel: close pop-up
            modalButton(label='Cancel')
          )
        ))
        
        # If they select a CoC that SOMEONE ELSE is associated with (by looking up CoC Instances joined with CoC Instance Users where the user is NOT this user and user role = "Admin"), 
      } else if(nrow(check_if_others_have) > 0){
        admin_email(check_if_others_have$username)
        instance_requested(check_if_others_have$coc_instance_id)
        # let them know as much and provide them the option to "Request Access" or "Create ORR anyway"
        showModal(modalDialog(
          title = 'Other owned CoC Instance Found',
          helpText(paste0('Another user (', check_if_others_have$username ,') has an existing instance for CoC: ', coc_requested(),'. Would you like to request
                          access from this user, or continue creating a new instance for this CoC?')),
          footer = tagList(
            actionButton(ns('request_access'), label='Request Access'),
            # If they continue: go to next step
            actionButton(ns('continue_new_instance2'), label='Create ORR anyway')
          )
        ))
       
      } else {
        admin_email(NULL)
        instance_requested(NULL)
        # If they select a CoC that has no other CoC Instances: go to next step
        showModal(modalDialog(
          title = 'No Existing CoC Instances Found',
          helpText(paste0('There are no an existing instances for CoC: ', coc_requested(),'. 
                          You will become the admin for this CoC instance upon creation. Would you like to continue?')),
          footer = tagList(
            # If they continue: go to next step
            actionButton(ns('continue_new_instance3'), label='Continue'),
            # If they cancel: close pop-up
            modalButton(label='Cancel')
          )
        ))
      }
   
    })
    
    
    observeEvent(input$continue_new_instance,{
        
      removeModal()
        showModal(
          modalDialog(
            title = 'New Instance Dataset',
            radioButtons(ns('hic_import_select'),
                         label = 'Which version of the HIC data would you like to use?',
                         choices = list(
                           'Import the HIC data as of X/X/XX' = "import",
                           'Upload my own version of the HIC data' = "upload"
                         )
            ),
            uiOutput(ns('hic_cond_select')),
            footer = tagList(
              actionButton(inputId=ns('new_hic_instance'),label='Create New Instance'),
              modalButton(label='Cancel')
            )
          ),
          session = session
        )
                  
    })
    
    observeEvent(input$continue_new_instance2,{
      removeModal()
      showModal(
        modalDialog(
          title = 'New Instance Dataset',
          radioButtons(ns('hic_import_select'),
                       label = 'Which version of the HIC data would you like to use?',
                       choices = list(
                         'Import the HIC data as of X/X/XX' = "import",
                         'Upload my own version of the HIC data' = "upload"
                       )
          ),
          uiOutput(ns('hic_cond_select')),
          footer = tagList(
            actionButton(inputId=ns('new_hic_instance'),label='Create New Instance'),
            modalButton(label='Cancel')
          )
        ),
        session = session
      )
    })
    
    observeEvent(input$continue_new_instance3,{
      removeModal()
      showModal(
        modalDialog(
          title = 'New Instance Dataset',
          radioButtons(ns('hic_import_select'),
                       label = 'Which version of the HIC data would you like to use?',
                       choices = list(
                         'Import the HIC data as of X/X/XX' = "import",
                         'Upload my own version of the HIC data' = "upload"
                       )
          ),
          uiOutput(ns('hic_cond_select')),
          footer = tagList(
            actionButton(inputId=ns('new_hic_instance'),label='Create New Instance'),
            modalButton(label='Cancel')
          )
        ),
        session = session
      )
    })
    
    # If they "Request Access": 
    # send email to user associated with that other CoC Instance
    observeEvent(input$request_access, {
      req(!is.null(admin_email()))
      
      ## TODO: send email to admin of instance that is requested
      
      
      showModal(
        modalDialog(
          title = 'Request Sent',
          helpText('Thank you. A request has been sent to the Admin for this instance.'),
          HTML(paste0('<p>Request Details</p>
               <ul>
               <li>Requested CoC: ', input$coc_dropdown,'</li>',
               '<li>Requested instance: ','</li>',    
               '<li>Requested at: ',Sys.time(),'</li>',
               '</ul>
               '))
        )
      )
    })
    
    observeEvent(input$new_hic_instance, {
      req(input$hic_import_select == 'import')
      
      # If they choose to import: create a new CoC Instance and corresponding CoC Instance User with CoC Role = Admin
      new_instance <- 
        data.table(coc_instance_id = max(coc_iu()$coc_instance_id) + 1, 
                   coc_instance_name = paste0(input$coc_dropdown, '-', str_to_upper(user_coc$given_name)),
                   coc = input$coc_dropdown, 
                   ## reference_id for 'Not Started'
                   coc_status = 8,
                   date_created = Sys.time(), created_by = user_coc$email, 
                   date_updated = Sys.time(), updated_by = user_coc$email)
      
      dbWriteTable(
        conn = DB_CON,name = 'coc_instances',new_instance,
        append = TRUE
      )
      
      new_instance_user <- data.table(
        coc_instance_user_id = max(coc_iu()$coc_instance_user_id) + 1,
        coc_instance_id = new_instance$coc_instance_id,
        username = user_coc$email,
        ## reference_id for "Admin"
        coc_instance_role = 5,
        new_instance %>% fselect(date_created, created_by, date_updated, updated_by, coc)
      )
      
      dbWriteTable(
        conn = DB_CON,name = 'coc_instance_users',new_instance_user,
        append = TRUE
      )
      
      coc_iu(
        rbind(copy(coc_iu(), new_instance_user), fill=TRUE)
      )
      
      shiny::showNotification('New CoC instance created!', type='message')
      removeModal()
      
    })
    
    
    
    get_hic_data <- function() {
      bed_field_mapping <- c(
        all_fam_beds = "beds_hh_w_children", 
        ch_fam_beds = "ch_beds_hh_w_children",
        vet_fam_beds = "veteran_beds_hh_w_children", 
        par_youth_beds = "youth_beds_hh_w_children",
        vet_ind_beds = "veteran_beds_hh_wo_children",
        single_youth_beds = "youth_beds_hh_wo_children"
      )

      coc_data <- get_db_tbl("all_hic_data") |>
        fsubset(hudnum == user_coc$coc) 

      project_data <- coc_data %>%
        fmutate(
          project_id = seq_row(.),
          mckinneyvento = factor_yesno(rowSums(gvr(., "mckinneyvento"), na.rm = TRUE) > 0),
          mckinneyventoyhdp = factor_yesno(mckinneyventoyhdp),
          dv_renewal = factor_yesno(NA),
          grant_number = as.character(NA), 
          coc_amount_awarded_last_year = as.numeric(NA),
          coc_amount_expended_last_year = as.numeric(NA),
          coc_funding_requested = as.numeric(NA),
          funding_action = fifelse(mckinneyvento == "Yes", "Renew", "Ignore"),
          coc_instance_id = user_coc$coc_instance_id,
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
