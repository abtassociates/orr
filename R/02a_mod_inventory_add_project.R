LOOKUP_CHOICES <- list(
  funding_action = setdiff(names(get_labelled_lookups("funding_action")), "Ignore"),
  reallocation_funding_actions = c("New", "Expand"),
  all_project_types =  DBI::dbGetQuery(
    DB_CON, 
    "SELECT value FROM lookups WHERE reference_type = 'project_type'"
  )$value,
  coc_renewal_reallocate_types = c("PSH", "TH", "RRH", "TH+RRH", "SSO", "HMIS"),
  coc_new_expansion_types = c("PSH", "TH", "RRH", "TH+RRH", "SSO - CE", "HMIS"),
  yhdp_project_types = c("PSH", "RRH", "TH", "TH+RRH", "SSO - CE"), # what about SSO-Host Homes?
  dv_project_types = c("RRH", "TH+RRH", "SSO - CE"),
  dv_reallocation_project_types = c("RRH", "TH+RRH", "SSO - CE"),
  funding_source = c("CoC","YHDP","DV"),
  no_yhdp_funding_source = c("CoC", "DV"),
  target_populations = names(get_labelled_lookups("target_population"))#c("DV","HIV","Youth", "General") # AS 8/26: What are the right populations here?
)

# ===================================================================
# UI Function (Now with a helper for bed inputs)
# ===================================================================
mod_inventory_add_project_ui <- function(id, form_type = "New", project_to_replace = NULL, orgnames) {
  ns <- NS(id)
  
  title <- switch(form_type,
                  "YHDP Reallocation" = "YHDP Reallocation Form",
                  "DV Reallocation"   = "DV Reallocation Form",
                  "CoC Reallocation"  = "CoC Reallocation Form",
                  "YHDP Replacement"  = "YHDP Replacement Form",
                  "Add New Project"
  )
  
  # Helper to create a group of bed inputs, wrapped in a div for easy toggling
  bed_input_group <- function(group_id, fam_label, ind_label) {
    div(
      id = ns(group_id),
      layout_columns(
        numericInput(ns(paste0(group_id, "_fam")), fam_label, value = 0, min = 0),
        numericInput(ns(paste0(group_id, "_ind")), ind_label, value = 0, min = 0),
        col_widths = c(6, 6)
      )
    )
  }
  
  modalDialog(
    title = title,
    size = "xl",
    page_fluid(
      # -- Core Project Info --
      layout_columns(
        
        # First column
        div(
          style = "padding-right: 5px; margin-right: -5px;",
        layout_columns(  
        div(
          textInput(ns("project_name"), "Project Name*",placeholder = "Please enter a name"),
          selectInput(ns("funding_source"), "Funding Source*", selectize = TRUE, choices = c("Select an option below" = "", LOOKUP_CHOICES$funding_source)),
          selectInput(ns("project_type"), "Project Type*", selectize = TRUE, choices = c("Select an option below" = "", LOOKUP_CHOICES$all_project_types)), # Choices populated by server
          textInput(ns("grant_number"), "Grant Number", placeholder = "Please enter if applicable") # Visibility controlled by server
        ),
        # Second column  
        div(
          selectizeInput(ns("organization_name"), label = "Organization Name*", choices = orgnames, options=list(create=TRUE)), 
          selectInput(ns("funding_action"), "Funding Action*", selectize = TRUE, choices = c("Select an option below" = "", LOOKUP_CHOICES$funding_action)),
          selectInput(ns("target_population"), "Target Population*", selectize = TRUE, choices = c("Select an option below" = "", LOOKUP_CHOICES$target_populations)),
        )
        ),
        col_widths = c(6,6)
        ),
        div(
          style = "border-left: 1px solid gray; padding-left: 10px; margin-left: -5px;",
          
          # -- Bed Fields (Generated programmatically) --
          bed_input_group("total_beds", "Total Family Beds*", "Total Individual Beds*"),
          bed_input_group("ch_beds", "CH Family Beds*", "CH Individual Beds*"),
          bed_input_group("vet_beds", "Veteran Family Beds*", "Veteran Individual Beds*"),
          bed_input_group("youth_beds", "Parenting Youth Beds*", "Single Youth Beds*"),
          
          # -- PSH-specific checkboxes --
          shinyjs::hidden(
            div(id = ns("ch_checkbox_div"),
                checkboxInput(ns("targeted_ch_fam"), "100% of family beds targeted to CH", FALSE),
                checkboxInput(ns("targeted_ch_ind"), "100% of individual beds targeted to CH", FALSE)
            )
          ),
        ),
        col_widths = c(6,6)
      ), 
        
      #shinyjs::hidden(helpText(id = ns("target_population_inst"), "Select if project is targeted to DV, HIV, Youth, or General")),
      shinyjs::hidden(checkboxInput(ns("all_dv_checkbox"), "100% targeted to DV", value = FALSE)),
      
    ),
    footer = tagList(
      div(style = "width: 50%; text-align:left;",
           tags$p("* = Required field"),
           #tags$p("^ See Appendix E for YHDP project type mapping")
       ),
      
      actionButton(ns("cancel"), label="Cancel"),
      actionButton(ns("submit"), "Submit project", class = "btn-primary", icon = icon('check')),
      actionButton(ns("add_another_link"), "Submit and add another project", class = "btn-primary", icon = icon('check-double')),
      
    )
  )
}

# ===================================================================
# Server Function (Refactored for clarity and consolidation)
# ===================================================================
mod_inventory_add_project_server <- function(
    id, 
    trigger,
    form_type = "New", 
    funding_source = "", 
    project_to_replace = NULL, 
    user_coc = NULL,
    orgnames = NULL,
    parent_session = NULL
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    modal_submission_outcome <- reactiveValues(
      status = NULL,
      project_data = NULL
    )
    add_another_flag <- reactiveVal(FALSE)
    
    # =================================================================
    # 1. Centralized State Logic (Reactive Expressions)
    # These reactives define the form's state based on user inputs.
    # =================================================================
    # --- The Pre-population Logic ---
    
    reset_form <- function() {
      updateTextInput(session, "project_name", value = "")
      updateSelectInput(session, "funding_source", selected = "")
      updateTextInput(session, "organization_name", value = "")
      updateSelectInput(session, "funding_action", selected = "")
      updateNumericInput(session, "youth_beds_fam", value = "")
      updateNumericInput(session, "youth_beds_ind", value = "")
      updateNumericInput(session, "total_beds_fam", label = if (current_funding_source() == "DV") "DV Family Beds*" else "Total Family Beds*", value = "")
      updateNumericInput(session, "total_beds_ind", label = if (current_funding_source() == "DV") "DV Individual Beds*" else "Total Individual Beds*", value ="")
      updateTextInput(session, "grant_number", label = if (fa == "Replace") "Grant Number*" else "Grant Number", value = "")
      updateCheckboxInput(session, "all_dv_checkbox", value = FALSE)
      updateSelectInput(session, "target_population", selected = "tp")
    }
    # This fires whenever the 'trigger' is incremented in the parent
    observeEvent(trigger(), {
      req(trigger() > 0)
      
      # Clear existing validation errors
      iv$disable() 
      
      # Logic for 'Replace' or 'Reallocate' prepopulation
      if (grepl("Reallocation", form_type) && funding_source != "") {
        updateSelectInput(session, "funding_source", selected = funding_source)
        updateSelectInput(session, "funding_action", choices = c("Select an option below" = "", LOOKUP_CHOICES$reallocation_funding_actions))
        if (funding_source == "YHDP") {
          updateSelectInput(session, "funding_action", selected = "New")
        }
        #updateActionLink(session, "add_another_link", label = "Submit and add another reallocation project?")
        shinyjs::hide("grant_number") # should never need grant number because can only reallocate to New or Expand
        shinyjs::disable("funding_source")
      } else if (form_type() == "YHDP Replacement" && !is.null(project_to_replace())) {
        updateTextInput(session, "project_name", value = project_to_replace$`Project Name`)
        #updateTextInput(session, "organization_name", value = project_to_replace$`Organization Name`)
        updateSelectInput(session, "funding_action", selected = "Replace")
        updateNumericInput(session, "youth_beds_fam", value = project_to_replace$`Par Youth Beds`)
        updateNumericInput(session, "youth_beds_ind", value = project_to_replace$`Single Youth Beds`)
        #updateActionLink(session, "add_another_link", label = "Submit and add another replacement project?")
        shinyjs::disable("funding_source")
      } else {
        reset_form()
      }
    })
    
      #updateSelectInput(session, "organization_name", choices = orgnames)
    
    # Determine the definitive target population.
    current_target_pop <- reactive({
      if (current_funding_source() == "YHDP") "Yth" 
      else if (current_funding_source() == "DV") "DV" 
      else if(is.null(input$target_population) || input$target_population == "") ""
      else input$target_population
    })
    
    # Determine which bed groups should be visible. This is the core of the display logic.
    visible_bed_groups <- reactive({
      pt <- input$project_type
      tp <- current_target_pop()
      
      # Hide all beds for certain project types
      if (isTruthy(pt) && pt %in% c("SSO", "SSO - CE", "SSO-Host Homes", "HMIS")) {
        return(character(0))
      }

      # Determine visibility based on funding and population
      groups <- c()
      if (current_funding_source() == "YHDP") groups <- c("youth_beds")
      else if (current_funding_source() == "DV") groups <- c("total_beds") # Will be relabeled to "DV Beds"
      else { # CoC logic
        groups <- c("total_beds", "vet_beds")
        if (tp == "Yth" || tp == "") groups <- c(groups, "youth_beds")
        if (pt == "PSH" || is.null(pt) || pt == "") groups <- c(groups, "ch_beds")
      }
      return(groups)
    })
    
    # =================================================================
    # 2. Initial Form Setup (Runs once)
    # =================================================================
    
    # Set initial values for reallocation or replacement
    # if (grepl("Reallocation", form_type) && funding_source != "") {
    #   updateSelectInput(session, "funding_source", selected = funding_source)
    #   updateSelectInput(session, "funding_action", choices = c("Select an option below" = "", LOOKUP_CHOICES$reallocation_funding_actions))
    #   if (funding_source == "YHDP") {
    #     updateSelectInput(session, "funding_action", selected = "New")
    #   }
    #   #updateActionLink(session, "add_another_link", label = "Submit and add another reallocation project?")
    #   shinyjs::hide("grant_number") # should never need grant number because can only reallocate to New or Expand
    #   shinyjs::disable("funding_source")
    # } else if (form_type == "YHDP Replacement" && !is.null(project_to_replace)) {
    #   updateTextInput(session, "project_name", value = project_to_replace$`Project Name`)
    #   #updateTextInput(session, "organization_name", value = project_to_replace$`Organization Name`)
    #   updateSelectInput(session, "funding_action", selected = "Replace")
    #   updateNumericInput(session, "youth_beds_fam", value = project_to_replace$`Par Youth Beds`)
    #   updateNumericInput(session, "youth_beds_ind", value = project_to_replace$`Single Youth Beds`)
    #   #updateActionLink(session, "add_another_link", label = "Submit and add another replacement project?")
    #   shinyjs::disable("funding_source")
    # }
    
    current_funding_source <- reactive({
      ifelse(funding_source != "", funding_source,
             ifelse(!is.null(input$funding_source), input$funding_source, ""))
      
    })
    # =================================================================
    # 3. Dynamic UI Updater (Single Observer)
    # This observer updates the entire UI based on the state reactives.
    # =================================================================
    # Update project type choices when funding source or action changes
    observeEvent(c(input$funding_action, input$funding_source), {
      fa <- if(is.null(input$funding_action)) "" else input$funding_action

      proj_type_choices <- 
        if (current_funding_source() == "CoC" && fa %in% c("Renew", "Reallocate")) LOOKUP_CHOICES$coc_renewal_reallocate_types
        else if (current_funding_source() == "CoC") LOOKUP_CHOICES$coc_new_expansion_types
        else if (current_funding_source() == "YHDP") LOOKUP_CHOICES$yhdp_project_types
        else if (current_funding_source() == "DV" && fa == "Reallocate") LOOKUP_CHOICES$dv_reallocation_project_types
        else if (current_funding_source() == "DV") LOOKUP_CHOICES$dv_project_types
        else LOOKUP_CHOICES$all_project_types
      
      updateSelectInput(session, "project_type", choices = c("Select Funding Source first" = "", proj_type_choices), selected = input$project_type)
    }, ignoreInit = FALSE, ignoreNULL = FALSE)
    
    # Update visibility based on funding action
    observeEvent(input$funding_action, {
      fa <- input$funding_action
      is_new_or_expand <- fa %in% c("New", "Expand")
      
      shinyjs::toggle("grant_number", condition = !is_new_or_expand & !grepl("Reallocation", form_type))
      # shinyjs::toggleState("funding_source", condition = fa != "Replace" && !grepl("Reallocation", form_type))
      # shinyjs::toggleState("funding_action", condition = fa != "Replace" && !(current_funding_source() == "YHDP" && grepl("Reallocation", form_type)))
    }, ignoreInit = TRUE, ignoreNULL = FALSE)
    
    # Update bed group visibility
    observeEvent(c(input$project_type, current_target_pop()), {
      vis_beds <- visible_bed_groups()
      all_bed_groups <- c("total_beds", "ch_beds", "vet_beds", "youth_beds")
      lapply(all_bed_groups, function(group) shinyjs::toggle(group, condition = group %in% vis_beds))
    }, ignoreInit = FALSE, ignoreNULL = FALSE)
    
    # Update DV checkbox and related elements
    observeEvent(c(input$funding_action, input$project_type, current_target_pop()), {
      fa <- if(is.null(input$funding_action)) "" else input$funding_action
      pt <- if(is.null(input$project_type)) "" else input$project_type
      tp <- current_target_pop()
      
      is_new_or_expand <- fa %in% c("New", "Expand")
      show_dv_check <- is_new_or_expand && (
        (isTruthy(pt) && pt == "SSO - CE") || 
          (tp == "DV" && isTruthy(pt) && pt %in% c("RRH", "TH+RRH"))
      )
      # if(!is_new_or_expand){
      #   updateTextInput(session, "grant_number", label = if (fa == "Replace") "Grant Number*" else "Grant Number")
      # }
      shinyjs::toggle("all_dv_checkbox", condition = show_dv_check)
      if(current_funding_source() == "DV") updateCheckboxInput(session, "all_dv_checkbox", value = TRUE)
      #shinyjs::toggle("target_population_inst", condition = current_funding_source() == "CoC" && !show_dv_check)
      shinyjs::toggle("ch_checkbox_div", condition = current_funding_source() == "CoC" && isTruthy(pt) && pt == "PSH")
      shinyjs::toggleState("all_dv_checkbox", condition = show_dv_check && current_funding_source() != "DV")
      
      # Missing organization_name state control
      shinyjs::toggleState("organization_name", condition = form_type != "YHDP Replacement")
    }, ignoreInit = FALSE, ignoreNULL = FALSE)
    
    # Update labels based on funding source and action
    observeEvent(c(input$funding_action), {
      fa <- if(is.null(input$funding_action)) "" else input$funding_action

      updateNumericInput(session, "total_beds_fam", label = if (current_funding_source() == "DV") "DV Family Beds*" else "Total Family Beds*")
      updateNumericInput(session, "total_beds_ind", label = if (current_funding_source() == "DV") "DV Individual Beds*" else "Total Individual Beds*")
      #updateTextInput(session, "grant_number", label = if (fa == "Replace") "Grant Number*" else "Grant Number")
    }, ignoreInit = FALSE, ignoreNULL = FALSE)
    
    # Update Target Population Selection
    observeEvent(current_target_pop(), {
      tp <- current_target_pop()

      shinyjs::toggleState("target_population", condition = TRUE) # start by enabling so we can set the value
      if (current_funding_source() != "CoC") updateSelectInput(session, "target_population", selected = tp)
      shinyjs::toggleState("target_population", condition = current_funding_source() %in% c("", "CoC"))
    }, ignoreInit = FALSE, ignoreNULL = FALSE)
    
    # --- PSH Checkbox Logic ---
    observeEvent(input$targeted_ch_fam, 
                 if(isTRUE(input$targeted_ch_fam)) 
                   updateNumericInput(session, "ch_beds_fam", value = input$total_beds_fam)
    )
    observeEvent(input$targeted_ch_ind, 
                 if(isTRUE(input$targeted_ch_ind)) 
                   updateNumericInput(session, "ch_beds_ind", value = input$total_beds_ind)
    )
    
    observeEvent(c(input$total_beds_fam, input$ch_beds_fam), {
      is_equal <- input$total_beds_fam > 0 && input$total_beds_fam == input$ch_beds_fam
      updateCheckboxInput(session, "targeted_ch_fam", value = is_equal)
    }, ignoreNULL = FALSE)
    observeEvent(c(input$total_beds_ind, input$ch_beds_ind), {
      is_equal <- input$total_beds_ind > 0 && input$total_beds_ind == input$ch_beds_ind
      updateCheckboxInput(session, "targeted_ch_ind", value = is_equal)
    }, ignoreNULL = FALSE)
    
    # =================================================================
    # 4. Validation & Submission
    # =================================================================
    iv <- shinyvalidate::InputValidator$new()

    # Core fields are always required
    iv$add_rule("project_name", sv_required())
    iv$add_rule("organization_name", sv_required())
    iv$add_rule("funding_action", sv_required())
    iv$add_rule("project_type", sv_required())
    iv$add_rule("funding_source", sv_required())
    iv$add_rule("target_population", sv_required())
    
    # Grant number is only required if not New or Expand
    grant_iv <- shinyvalidate::InputValidator$new()
    grant_iv$add_rule("grant_number", sv_required())
    grant_iv$condition( ~ !input$funding_action %in% c("New", "Expand"))
    iv$add_validator(grant_iv)
    
    # Validation for visible bed fields
    bed_groups_to_validate <- list(
      total_beds = c("total_beds_fam", "total_beds_ind"),
      ch_beds = c("ch_beds_fam", "ch_beds_ind"),
      vet_beds = c("vet_beds_fam", "vet_beds_ind"),
      youth_beds = c("youth_beds_fam", "youth_beds_ind")
    )

    # subset reactive checks if subset field is > total field (e.g., CH In beds <= Total Ind beds)
    sv_lte_reactive <- function(max_val_formula) {
      force(max_val_formula)
      max_val_func <- rlang::as_function(max_val_formula)
      function(value) {
        # Use rlang::eval_tidy() to get the CURRENT value from the formula
        max_val <- max_val_func()
        
        # Don't show an error if either value is missing
        if (is.null(max_val) || is.na(max_val) || is.na(value)) {
          return(NULL)
        }
        
        if (value > max_val) {
          paste0("Cannot be greater than the total (", max_val, ")")
        } else {
          NULL # Return NULL for success
        }
      }
    }
    # Loop to create and add all bed validation rules ONCE
    for (group_name in names(bed_groups_to_validate)) {
      # Use local() to capture the current value of 'group_name' for the condition formula
      local({
        # 'current_group' is a new variable for each loop iteration
        current_group <- group_name
        
        # Create a new validator for this group
        v <- shinyvalidate::InputValidator$new()
        
        # Set the condition for the ENTIRE validator group.
        # This is much more efficient than setting it inside an observe().
        v$condition(~ current_group %in% visible_bed_groups())
        
        # Add rules for the fields within this group
        for (field in bed_groups_to_validate[[current_group]]) {
          v$add_rule(field, sv_required())
          v$add_rule(field, sv_integer("Must be a whole number"))
          v$add_rule(field, sv_gte(0, "Cannot be negative"))
        }
        
        # Subset validation
        if (current_group != "total_beds") {
          v$add_rule(paste0(current_group, "_fam"), sv_lte_reactive(~input$total_beds_fam))
          v$add_rule(paste0(current_group, "_ind"), sv_lte_reactive(~input$total_beds_ind))
        }
        
        # Add this group's validator to the main validator
        iv$add_validator(v)
      })
    }

    # --- If they cancel, regardless of previous submission attempts, disable the iv
    observeEvent(input$cancel, {
      add_another_flag(FALSE)
      removeModal()
      iv$disable()
      modal_submission_outcome <- NULL
    }, ignoreInit = TRUE)
    
    observeEvent(input$add_another_link, {
        print('observed input$add_another_link')
        add_another_flag(TRUE)
      # This action sets a flag and then programmatically clicks the main submit button.
      # This allows us to reuse the validation and submission logic from the submit button observer.
     
    }, ignoreInit = TRUE, priority = 2)
    
    
    # --- Submission Event ---
    observeEvent(c(input$submit, input$add_another_link), {
      req(isTruthy(input$submit) || isTruthy(input$add_another_link))
      iv$enable()
      print('observed input$submit')
      
      if (iv$is_valid()) {
        vis_beds <- visible_bed_groups()
        
        # Helper to get input value only if its group is visible
        get_val <- function(group, field_suffix) {
          if (group %in% vis_beds) input[[paste0(group, "_", field_suffix)]] else NA
        }
        
        # Handle YHDP Reallocation special case for total beds
        total_fam <- get_val("total_beds", "fam")
        total_ind <- get_val("total_beds", "ind")
        if (input$funding_source == "YHDP" && grepl("Reallocation", form_type)) {
          total_fam <- get_val("youth_beds", "fam")
          total_ind <- get_val("youth_beds", "ind")
        }

        # Collect data into a clean list
        new_project_data <- data.table(
          project_name = input$project_name,
          organization_name = input$organization_name,
          funding_action = input$funding_action,
          grant_number = ifelse(input$grant_number == "", NA, input$grant_number),
          project_type = input$project_type,
          target_population = input$target_population,
          is_dedicated_dv = input$all_dv_checkbox,
          dv_fam_beds = if(input$target_population == "DV") input$total_beds_fam,
          dv_ind_beds = if(input$target_population == "DV") input$total_beds_ind,
          all_fam_beds = input$total_beds_fam, 
          all_ind_beds = input$total_beds_ind,
          ch_fam_beds = input$ch_beds_fam, 
          total_ch_ind_beds = input$ch_beds_ind,
          vet_fam_beds = input$vet_beds_fam, 
          vet_ind_beds = input$vet_beds_ind,
          par_youth_beds = input$youth_beds_fam, 
          single_youth_beds = input$youth_beds_ind,
          is_dedicated_ch_fam = input$funding_source == "CoC" && input$project_type == "PSH" && input$targeted_ch_fam,
          is_dedicated_ch_ind = input$funding_source == "CoC" && input$project_type == "PSH" && input$targeted_ch_ind,
          mckinneyvento = TRUE,
          mckinneyventoyhdp = input$funding_source == "YHDP",
          dv_renewal = input$funding_source == "DV" && input$funding_action == "Renew",
          created_by = user_coc$username
        )
        # removeModal()
        modal_submission_outcome$project_data <- new_project_data
        
        # Logic for 'Submit and Add Another'
        if (input$add_another_link > 0) {
          modal_submission_outcome$status <- "add another"
          # Manually clear Name/Grant so user knows it's a new entry
          updateTextInput(session, "project_name", value = "")
          iv$disable() 
        } else {
          modal_submission_outcome$status <- "success"
          removeModal()
        }
        
        # Flush status so it can be triggered again by the same string
        shinyjs::delay(100, { modal_submission_outcome$status <- NULL })
        
        show_alert(
          title = "Success!",
          text = "The project was added successfully!",
          type = "success"
        )
      } else {
        add_another_flag(FALSE)
        show_alert(
          title = "Missing Required Fields",
          text = "Please correct the errors before submitting.",
          type = "error"
        )
        modal_submission_outcome <- NULL
      }
      print(paste0('done with input$submit, add_another_flag=', add_another_flag()))
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    
    # Return the reactiveVal to the parent module
    return(modal_submission_outcome)
  })
}