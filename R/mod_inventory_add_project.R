mod_inventory_add_project_ui <- function(id) {
  ns <- NS(id)
  
  fundingAction <- ns("funding_action_new")
  projectType   <- ns("project_type_new")
  fundingSource <- ns("funding_source_new")
  targetPop     <- ns("target_population_new")
  
  # Common condition for showing bed fields (most fields hidden if Replace, HMIS,
  # YHDP, DV, or specific project types)
  commonHideBedCond <- glue("
            input['{fundingAction}']=='Replace' || 
            input['{fundingSource}']=='YHDP' || 
            input['{fundingSource}']=='DV' || 
            input['{projectType}'].match(/^(SSO-CE|SSO|PSH|HMIS)$/)")
  
  # For fields that should also be hidden when Target Population is Youth:
  commonHideBedCondNoYouth <- glue("
            {commonHideBedCond} || input['{targetPop}']=='Youth')")
  
  # List of standard bed fields with their input IDs and labels
  bedFields <- list(
    list(id = "all_family_beds_new",      label = "All Family Beds *",         cond = !commonHideBedCond),
    list(id = "all_individual_beds_new",  label = "All Individual Beds *",     cond = !commonHideBedCond),
    list(id = "ch_family_beds_new",       label = "CH Family Beds *",          cond = !commonHideBedCondNoYouth),
    list(id = "ch_individual_beds_new",   label = "CH Individual Beds *",      cond = !commonHideBedCondNoYouth),
    list(id = "veteran_family_beds_new",  label = "Veteran Family Beds *",     cond = !commonHideBedCondNoYouth),
    list(id = "veteran_individual_beds_new", label = "Veteran Individual Beds *", cond = !commonHideBedCondNoYouth)
  )
  
  # Special condition: if Replace or funding source is YHDP, then show parenting and single youth beds.
  youthBedCond <- glue("(input['{fundingAction}']=='Replace' || input['{fundingSource}']=='YHDP')")
  
  # DV-specific condition
  dvBedCond <- glue("input['{fundingSource}']=='DV'")
  
  modalDialog(
    title = "Add New Project",
    size = "l",
    fluidPage(
      fluidRow(
        column(6, textInput(ns("project_name_new"), "Project Name *")),
        column(6, textInput(ns("organization_name_new"), "Organization Name *"))
      ),
      fluidRow(
        column(6, selectInput(fundingAction, "Funding Action",
                              choices = c("New (no historical performance)", "Replace", "Other"))),
        column(6, conditionalPanel(
          condition = glue("input['{fundingAction}'] != 'New (no historical performance)'"),
          textInput(ns("grant_number_new"), "Grant Number *")
        ))
      ),
      fluidRow(
        column(6, selectInput(fundingSource, "Funding Source",
                              choices = c("CoC", "YHDP", "DV"))),
        column(6, selectInput(projectType, "Project Type^",
                              choices = c("Default", "SSO-CE", "SSO", "HMIS", "PSH")))
      ),
      fluidRow(
        column(6, conditionalPanel(
          condition = glue("!(input['{fundingAction}']=='Replace' || input['{projectType}']=='HMIS')"),
          selectInput(targetPop, "Target Population",
                      choices = c("General", "Youth", "DV"))
        ))
      ),
      tags$hr(),
      # Generate the standard bed fields in a loop
      lapply(bedFields, function(field) {
        fluidRow(
          column(6, conditionalPanel(
            condition = sprintf(field$cond, fundingAction, projectType, fundingSource, fundingSource, projectType, targetPop),
            numericInput(ns(field$id), field$label, value = 0, min = 0)
          ))
        )
      }),
      # Youth bed fields (for Replace or YHDP)
      fluidRow(
        column(6, conditionalPanel(
          condition = youthBedCond,
          numericInput(ns("parenting_youth_beds_new"), "Parenting Youth Beds *", value = 0, min = 0)
        )),
        column(6, conditionalPanel(
          condition = youthBedCond,
          numericInput(ns("single_youth_beds_new"), "Single Youth Beds *", value = 0, min = 0)
        ))
      ),
      # DV-specific bed fields
      fluidRow(
        column(6, conditionalPanel(
          condition = dvBedCond,
          numericInput(ns("dv_family_beds_new"), "DV Family Beds *", value = 0, min = 0)
        )),
        column(6, conditionalPanel(
          condition = dvBedCond,
          numericInput(ns("dv_individual_beds_new"), "DV Individual Beds *", value = 0, min = 0)
        ))
      ),
      # PSH-specific checkboxes
      conditionalPanel(
        condition = glue("input['{projectType}']=='PSH'"),
        checkboxInput(ns("presh_family"), "100% of Family Beds targeted to CH", FALSE),
        checkboxInput(ns("presh_individual"), "100% of Individual Beds targeted to CH", FALSE)
      ),
      br(),
      # Additional reallocation project link
      actionLink(ns("reallocation_link"), "Is this an additional reallocation project?")
    ),
    footer = tagList(
      modalButton("Cancel"),
      actionButton(ns("submit_new_project"), "Submit")
    )
  )
}

mod_inventory_add_project_server <- function(id, projects_data) {
  moduleServer(id, function(input, output, session) {
    
    ## Submission ------------
    observeEvent(input$submit_new_project, {
      # Validate required fields
      req(input$project_name_new, input$organization_name_new)
      if (input$funding_action_new != "New (no historical performance)") {
        req(input$grant_number_new)
      }
      
      # Adjust funding source and target population per rules
      funding_source <- if (input$funding_action_new == "Replace") {
        "YHDP"
      } else {
        input$funding_source_new
      }
      
      youth_cond <- input$funding_action_new == "Replace" || funding_source == "YHDP"
      
      target_population <- if (youth_cond) "Youth"
        else if (funding_source == "DV") "DV"
        else input$target_population_new
      
      # Define a helper function to decide if a standard bed field should be captured.
      # These fields are hidden (and thus should be NA) if:
      #   - funding_action is "Replace"
      #   - project_type is HMIS, SSO-CE, SSO, or PSH
      #   - funding_source is YHDP or DV
      # For some fields, also hide when target_population is Youth.
      shouldCapture <- function(inputId, includeYouth = FALSE) {
        suppress <- youth_cond ||
          input$project_type_new %in% c("HMIS", "SSO-CE", "SSO", "PSH") ||
          funding_source == "DV"
        if (includeYouth) {
          suppress <- suppress || (input$target_population_new == "Youth")
        }
        return(!suppress)
      }
      
      # Use a list to map field names to their input IDs and whether they should hide if Target Population is Youth.
      bedSettings <- list(
        all_family_beds      = list(id = "all_family_beds_new",      youthSensitive = FALSE),
        all_individual_beds  = list(id = "all_individual_beds_new",  youthSensitive = FALSE),
        ch_family_beds       = list(id = "ch_family_beds_new",       youthSensitive = TRUE),
        ch_individual_beds   = list(id = "ch_individual_beds_new",   youthSensitive = TRUE),
        veteran_family_beds  = list(id = "veteran_family_beds_new",  youthSensitive = TRUE),
        veteran_individual_beds = list(id = "veteran_individual_beds_new", youthSensitive = TRUE)
      )
      
      # Loop over the standard bed fields, capturing the value if applicable.
      bedValues <- list()
      for (name in names(bedSettings)) {
        field <- bedSettings[[name]]
        bedValues[[name]] <- if (shouldCapture(field$id, includeYouth = field$youthSensitive)) {
          input[[ field$id ]]
        } else {
          NA
        }
      }
      
      # Process special bed fields.
      parenting_youth_beds <- if(youth_cond) input$parenting_youth_beds_new else NA
      single_youth_beds <- if(youth_cond) input$single_youth_beds_new else NA
      
      dv_family_beds <- if (funding_source == "DV") input$dv_family_beds_new else NA
      dv_individual_beds <- if (funding_source == "DV") input$dv_individual_beds_new else NA
      
      # Process PSH-specific checkboxes.
      psh_family_override <- if (input$project_type_new == "PSH") input$presh_family else NA
      psh_individual_override <- if (input$project_type_new == "PSH") input$presh_individual else NA
      
      # Build the new project row using a list (combining with the bedValues list)
      new_proj <- c(
        list(
          project_name      = input$project_name_new,
          organization_name = input$organization_name_new,
          grant_number      = if (input$funding_action_new != "New (no historical performance)") input$grant_number_new else NA,
          funding_action    = input$funding_action_new,
          funding_source    = funding_source,
          project_type      = input$project_type_new,
          target_population = target_population
        ),
        bedValues,
        list(
          parenting_youth_beds    = parenting_youth_beds,
          single_youth_beds       = single_youth_beds,
          dv_family_beds          = dv_family_beds,
          dv_individual_beds      = dv_individual_beds,
          psh_family_override     = psh_family_override,
          psh_individual_override = psh_individual_override
        )
      )
      
      # Convert the new project to a data.frame and append it to the projects_data
      new_row_df <- as.data.frame(new_proj, stringsAsFactors = FALSE)
      updated <- rbind(projects_data(), new_row_df)
      projects_data(updated)
      
      removeModal()
    })
  })
}