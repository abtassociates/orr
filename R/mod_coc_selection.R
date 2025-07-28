mod_coc_selection_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    "Select CoC",
    value = id,
    card(
      card_header("Select a CoC Instance to Edit"),
      card_body(
        fillable = FALSE,
        # a "Create" button or link above the table will display so they can create a new CoC Instance
        actionButton(ns('create_new_instance'), "Create a New CoC Instance"),
        selectInput(ns('choose_user'), "Select a User Profile",  choices=users$username),
        DTOutput(ns('coc_instances_dt')),
        actionButton(ns('edit_coc_instance'),"Edit selected CoC", icon = icon('edit'))
      )
    )
  )
}

mod_coc_selection_server <- function(id, nav_control, projects_data, selected_coc, coc_instance_id) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    ## subset coc_instance_users to specific user
    coc_iu <- reactive({
      coc_instance_users[username == input$choose_user]
    })
    
    
    ## disable Edit button unless row is selected
    observe({
      if(length(input$coc_instances_dt_rows_selected)==0){
        shinyjs::disable(id = 'edit_coc_instance')
      } else {
        shinyjs::enable(id = 'edit_coc_instance')
      }
    })
    
    output$coc_instances_dt <- renderDT({
      
      datatable(coc_iu(), 
                options = list(dom = 'tpi'),
                editable = FALSE,
                style = 'default',
                filter = list(position = 'top', plain = TRUE),
                selection = 'single'
      )
    })
    
    observeEvent(input$edit_coc_instance, {
      selected_coc(coc_iu()$coc[[1]])
      print(selected_coc())
      
      coc_instance_id(
        coc_iu()[input$coc_instances_dt_rows_selected, .(coc_instance_id)][[1]]
      )
      
      nav_control("inventory")
      
      # Initialize projects data
      filtered_data <- get_hic_data()
      
      projects_data(filtered_data)
    })
    output$new_instance_ui <- renderUI({
      # If they select a CoC that has existing CoC Instances
      if(1 == 1){}
      # If there’s an instance THEY are already associated with (by looking up CoC Instances joined with CoC Instance Users where the user is this user), warn them that they already have an ORR for this CoC and that if they wish to modify settings, they can do so within existing ORRs. Show options "Continue" or "Cancel"
      # If they continue: go to next step
      # If they cancel: close pop-up
      # If they select a CoC that SOMEONE ELSE is associated with (by looking up CoC Instances joined with CoC Instance Users where the user is NOT this user and user role = "Admin"), let them know as much and provide them the option to "Request Access" or "Create ORR anyway"
      # If they "Request Access": 
      # send email to user associated with that other CoC Instance
      # If the user accepts, create a new CoC Instance User with role ( "Editor" or "Viewer")
      # provide feedback that email was sent and that they will be alerted via email if/when their request was accepted or rejected. close the modal
      # If they "Create ORR anyway": go to next step
      # If they select a CoC that has no other CoC Instances: go to next step
      
    })
    
    #  When they hit Create: display pop-up form titled "Create ORR" with a simple dropdown to select a CoC.
    observeEvent(input$create_new_instance, {
      showModal(
        modalDialog(
          title = 'Create ORR',
          selectInput(ns('coc_dropdown'),
                      label = "Please choose a CoC:",
                      choices = setNames(cocs$coc_name,nm = cocs$coc_code)
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
        # If they choose to import: create a new CoC Instance and corresponding CoC Instance User with CoC Role = Admin
        
      }
    })
    # Once they select a CoC, close the previous modal and show another one with a radio button asking if they want to import the HIC data or import the HIC data as of X/X/XX date.
    observeEvent(input$choose_coc, {
      
      showModal(
        modalDialog(
          radioButtons(ns('hic_import_select'),
                       label = 'Which version of the HIC data would you like to use?',
                       choices = c(
                         import = 'Import the HIC data as of X/X/XX',
                         upload = 'Upload my own version of the HIC data'
                       )
          ),
          uiOutput(ns('hic_cond_select')),
          footer = modalButton('Create New Instance')
        ),
        session = session
      )
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
        fsubset(hudnum == selected_coc()) 

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
          funding_action = fifelse(
            mckinneyvento == "Yes", 
            lookups$funding_actions[funding_action == "Renew", funding_action_id], 
            lookups$funding_actions[funding_action == "Ignore", funding_action_id]
          ),
          coc_instance_id = coc_iu()$coc_instance_id,
          # additional cols user will fill out
          is_dedicated_ch_fam = factor_yesno(NA),
          is_dedicated_ch_ind = factor_yesno(NA),
          is_dedicated_dv = factor_yesno(NA),
          amount_other_public_funding = as.numeric(NA),
          amount_private_funding = as.numeric(NA)
        ) %>%
        frename(bed_field_mapping) %>%
        fmutate(
          all_ind_beds = beds_hh_wo_children + beds_hh_w_only_children,
          total_ch_ind_beds = ch_beds_hh_wo_children + ch_beds_hh_w_only_children,
          dv_fam_beds = fifelse(target_population == "DV", all_fam_beds, as.integer(0)),
          dv_ind_beds = fifelse(target_population == "DV", all_ind_beds, as.integer(0))
        ) %>%
        fmutate(
          funding_action = convert_to_factor(., "funding_action"),
          project_type = convert_to_factor(., "project_type", textToNum = TRUE),
          target_population = convert_to_factor(., "target_population", textToNum = TRUE),
          dv_renewal = factor_yesno(dv_renewal)
        ) %>%
        get_vars(dbListFields(DB_CON, "projects"))
      
      return(project_data)
    }
    
    convert_to_factor <- function(data, v, textToNum = FALSE) {
      lookup_info <- lookups[[pluralize(v)]]
      id_col <- glue::glue("{v}_id")

      factor(
        if(!textToNum) data[[v]] else lookup_info[[id_col]][match(data[[v]], lookup_info[[v]])],
        levels = lookup_info[[id_col]],
        labels = lookup_info[[v]]
      )
    }
  })
}