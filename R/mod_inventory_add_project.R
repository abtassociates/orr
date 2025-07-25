mod_inventory_add_project_ui <- function(id) {
  ns <- NS(id)
  
  fundingAction <- ns("funding_action_new")
  projectType   <- ns("project_type_new")
  fundingSource <- ns("funding_source_new")
  targetPop     <- ns("target_population_new")
  
  # Common condition for showing bed fields (most fields hidden if Replace, HMIS,
  # YHDP, DV, or specific project types)
  commonHideBedCond <- glue::glue("
            input['{fundingAction}']=='Replace' || 
            input['{fundingSource}']=='YHDP' || 
            input['{fundingSource}']=='DV' || 
            input['{projectType}'].match(/^(SSO-CE|SSO|PSH|HMIS)$/)")
  
  # For fields that should also be hidden when Target Population is Youth:
  commonHideBedCondNoYouth <- glue::glue("
            {commonHideBedCond} || input['{targetPop}']=='Youth')")
  
  
  # Special condition: if Replace or funding source is YHDP, then show parenting and single youth beds.
  youthBedCond <- glue::glue("(input['{fundingAction}']=='Replace' || input['{fundingSource}']=='YHDP')")
  
  # DV-specific condition
  dvBedCond <- glue::glue("input['{fundingSource}']=='DV'")
  
  # List of standard bed fields with their input IDs and labels
  bedFields <- list(
    list(
      c1 = list(id = "all_family_beds_new", label = "All Family Beds*", cond = commonHideBedCond == FALSE),
      c2 = list(id = "all_individual_beds_new", label = "All Individual Beds*", cond = commonHideBedCond == FALSE)
    ),
    list(
      c1 = list(id = "ch_family_beds_new", label = "CH Family Beds*", cond = commonHideBedCondNoYouth == FALSE),
      c2 = list(id = "ch_individual_beds_new", label = "CH Individual Beds*", cond = commonHideBedCondNoYouth == FALSE)
    ),
    list(
      c1 = list(id = "veteran_family_beds_new", label = "Veteran Family Beds*", cond = commonHideBedCondNoYouth == FALSE),
      c2 = list(id = "veteran_individual_beds_new", label = "Veteran Individual Beds*", cond = commonHideBedCondNoYouth == FALSE)
    ),
    list(
      c1 = list(id = "parenting_youth_beds_new", label = "Parenting Youth Beds*", cond = youthBedCond),
      c2 = list(id = "single_youth_beds_new", label = "Single Youth Beds*", cond = youthBedCond)
    ),
    list(
      c1 = list(id = "dv_family_beds_new", label = "DV Family Beds*", cond = dvBedCond),
      c2 = list(id = "dv_individual_beds_new", label = "DV Individual Beds*", cond = dvBedCond)
    )
  )

  modalDialog(
    title = "Add Additional Project",
    size = "l",
    fluidPage(
      
      # Project and Org Names
      fluidRow(column(12, textInput(ns("project_name_new"), "Project Name *"))),
      textInput(ns("organization_name_new"), "Organization Name *"),
      
      # Funding Action
      selectizeInput(fundingAction, "Funding Action*", choices = c("", get_labelled_lookups("funding_action"))),
      
      # Grant Number
      conditionalPanel(
        condition = glue::glue("input['{fundingAction}'] != 'New (no historical performance)'"),
        textInput(ns("grant_number_new"), "Grant Number*")
      ),
      
      # Funding Source
      selectInput(fundingSource, "Funding Source*", choices = c("CoC", "YHDP", "DV")),
      
      # Project Type
      selectInput(projectType, "Project Type*^", choices = get_labelled_lookups("project_types")),
      
      # Target Population
      conditionalPanel(
        condition = glue::glue("!(input['{fundingAction}']=='Replace' || input['{projectType}']=='HMIS')"),
        selectInput(targetPop, "Target Population*", choices = get_labelled_lookups("target_populations"))
      ),
      tags$hr(),
      # Generate the standard bed fields in a loop
      lapply(bedFields, function(field_group) {
        fluidRow(
          lapply(field_group, function(field) {
            column(6, conditionalPanel(
              condition = field$cond,
              numericInput(ns(field$id), field$label, value=NULL, min = 0)
            ))
          })
        )
      }),
      
      # PSH-specific checkboxes
      conditionalPanel(
        condition = glue::glue("input['{projectType}']=='PSH'"),
        checkboxInput(ns("presh_family"), "100% of Family Beds targeted to CH", FALSE),
        checkboxInput(ns("presh_individual"), "100% of Individual Beds targeted to CH", FALSE)
      ),
      br(),
      # Additional reallocation project link
      actionLink(ns("reallocation_link"), "Is this an additional reallocation project?")
    ),
    footer = tagList(
      div(
        style = "width: 100%",
        tags$p("* means required"),
        tags$p("^ means 'See Appendix E in the instructions page for YHDP project type mapping'")
      ),
      modalButton("Cancel"),
      actionButton(ns("submit_new_project"), "Submit")
    )
  )
}

mod_inventory_add_project_server <- function(id, projects_data) {
  moduleServer(id, function(input, output, session) {
    ## Submission ------------
    # 1. Initialize the validator
    iv <- InputValidator$new()
    
    # 2. Add specific, conditional rules for each input
    # --- Core Project Info ---
    iv$add_rule("project_name_new", sv_required())
    iv$add_rule("organization_name_new", sv_required())
    iv$add_rule("funding_action_new", sv_required())
    iv$add_rule("funding_source_new", sv_required())
    iv$add_rule("project_type_new", sv_required())
    
    # --- Conditional Inputs ---
    # Grant Number is required ONLY IF funding action is not "New..."
    iv$add_rule("grant_number_new", 
                sv_required(test = function(value) {
                  input$funding_action_new != "New (no historical performance)"
                })
    )
    # Target Population is required ONLY IF it's not a special case
    iv$add_rule("target_population_new",
                sv_required(test = function(value) {
                  input$project_type_new == "HMIS"
                })
    )
    
    # --- Bed Fields ---
    # Define the conditions from your UI as R functions for re-use
    is_common_hidden <- function() {
      input$funding_action_new == 'Replace' || 
        input$funding_source_new %in% c('YHDP', 'DV') || 
        input$project_type_new %in% c('SSO-CE', 'SSO', 'PSH', 'HMIS')
    }
    is_common_no_youth_hidden <- function() {
      is_common_hidden() || (isTruthy(input$target_population_new) && input$target_population_new == 'Youth')
    }
    
    # All Beds
    iv$add_rule("all_family_beds_new", sv_required(test = function(value) !is_common_hidden()))
    iv$add_rule("all_individual_beds_new", sv_required(test = function(value) !is_common_hidden()))
    # CH Beds
    iv$add_rule("ch_family_beds_new", sv_required(test = function(value) !is_common_no_youth_hidden()))
    iv$add_rule("ch_individual_beds_new", sv_required(test = function(value) !is_common_no_youth_hidden()))
    # Veteran Beds
    iv$add_rule("veteran_family_beds_new", sv_required(test = function(value) !is_common_no_youth_hidden()))
    iv$add_rule("veteran_individual_beds_new", sv_required(test = function(value) !is_common_no_youth_hidden()))
    # Youth Beds
    iv$add_rule("parenting_youth_beds_new", sv_required(test = function(value) input$funding_action_new == 'Replace' || input$funding_source_new == 'YHDP'))
    iv$add_rule("single_youth_beds_new", sv_required(test = function(value) input$funding_action_new == 'Replace' || input$funding_source_new == 'YHDP'))
    # DV Beds
    iv$add_rule("dv_family_beds_new", sv_required(test = function(value) input$funding_source_new == 'DV'))
    iv$add_rule("dv_individual_beds_new", sv_required(test = function(value) input$funding_source_new == 'DV'))
    
    
    observeEvent(input$submit_new_project, {
      iv$enable()

      if (!iv$is_valid()) {
        showNotification(
          "Please fix the errors in the form before continuing",
          type = "error"
        )
        return()
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