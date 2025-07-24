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

mod_coc_selection_server <- function(id, nav_control, projects_data, selected_coc, con) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    ## subset coc_instance_users to specific user
    coc_iu <- reactive({
      coc_instance_users |>
        fsubset(username == input$choose_user)
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
      
      tmp <- coc_iu() |>
        join(coc_instances, on = 'coc_instance_id', how = 'left')
      
      selected_coc(as.vector(tmp[input$coc_instances_dt_rows_selected ,'coc_instance_id']))
      print(selected_coc())
      
      nav_control("inventory")
      
      # Initialize projects data
      filtered_data <- fake_projects |>
        fsubset(coc_instance_id == selected_coc()) |>
        fmutate(
          DV_Renewal = NA_character_,
          Grant_Number = NA_character_,
          CoC_Funding_Requested = NA_real_,
          Funding_Action = fifelse(McKinney_Vento == "No", "Ignore", "Renew")
        )
      
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
    
    return(coc_iu)
    
  })
}