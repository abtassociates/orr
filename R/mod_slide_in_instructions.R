mod_slide_in_instructions_ui <- function(id) {
  ns <- NS(id)
  
  # Instructions Sidebar
  div(
    id = ns("help_sidebar"),
    
    # The floating button attached to the outside
    actionLink(
      ns("toggle_help"), 
      label = icon("question-circle")
    ),
    
    # Inner container that handles the scrolling padding
    div(
      id = ns("help_sidebar_content"),
      uiOutput(ns("dynamic_help"))
    )
  )
}
  
mod_slide_in_instructions_server <- function(id, user_coc, nav_control) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    help_id <- reactiveVal("dashboard") # Default
    
    help_texts <- list(
      "dashboard"                                        = list(
        p('The My Dashboard page displays your Rating and Ranking tool versions and version access requests'),
        accordion(
          id = "my_accordion", # Optional: used to read the open state on the server
          open = FALSE,         # Set to FALSE to collapse everything by default
          
          accordion_panel(
            "Versions",
            p('To create a new version of the tool, select the "Create New Version" button, then choose your Continuum of Care (CoC) and import your most recent HIC to begin.'),
            p('To request access to a version created by someone else for your CoC, click the "Request Access to a CoC" button, select the CoC, and send a request to the original version creator. Multiple users can work together on the same tool at the same time. You will be able to see if they are on the same page as you at the top of the tool.'),
            p('To create a copy of an existing version, select the version in the table below and click "Copy Version". A CoC can have multiple versions of its Rating and Ranking tool to test different combinations of factors and parameters.'),
            p('Once the tool is created, you can access it by selecting the tool, then clicking the "Edit Selected Version" button, which will appear once the tool is selected.'),
            p('If you need to delete a version of the tool, you can do that by selecting the version and clicking the "Delete Selected Version" button.')
          ),
          
          accordion_panel(
            "Version Access Requests",
            p('All requests for access to the tool versions you created can be found in the Version Access Requests box.'),
            p('The "Received" section lists all pending access requests. Access requests can be approved or rejected. If access to a tool is granted, the requesting user will have the ability to edit the tool.'),
            p('The "Approved" section shows access requests you approved. Access requests you reject are listed in the "Rejected" section. The “Sent” section lists your own access requests and includes the status of those requests.')
          )
        )
      ),
        
      "inventory" = HTML("
        <p>The <strong>Review Projects</strong> page lists all Continuum of Care (CoC) projects that can be rated and ranked in this tool. You will want to review all projects listed and confirm that all information is correct.</p>
        
        <p>To make updates to a project, double-click within the cell you want to edit. Dark grey fields are calculated and are not editable.</p>
        
        <p>The following fields must be completed for full tool rating and ranking capabilities to be activated:</p>
        
        <ul>
          <li>DV Renewal</li>
          <li>Grant Number</li>
          <li>CoC Amount Awarded Last Operating Year</li>
          <li>CoC Amount Expended Last Operating Year</li>
          <li>CoC Funding Requested</li>
          <li>Funding Action</li>
        </ul>
        
        <p>To add a new project, click the <strong>Add New Project</strong> button in the upper right corner of the page. Complete all required fields, then click either <strong>Submit Project</strong> to return to your project list or <strong>Submit and Add Another Project</strong> to add another project. Fields may update as you select different <strong>Funding Actions</strong> and <strong>Target Populations</strong>.</p>
        
        <p><strong>HMIS</strong> and <strong>SSO-CE</strong> projects must be manually added to your project list so that they can be ranked later.</p>
        
        <p><em>Note:</em> Any changes made to projects listed in the tool will <strong>not</strong> be transferred back to your HIC.</p>"
      ),
      
      "funding_priorities" = list(
        HTML("<p>The <strong>Funding Ceilings + Priorities</strong> page allows you to specify system-wide funding priorities for your CoC's annual funding process.</p>"),
        accordion(
          accordion_panel(
            "General Funding Information",
            p("Your CoC's <strong>Annual Renewal Demand (ARD)</strong> amount is based on HUD's Estimated Annual Renewal Demand Report. If the numbers do not match HUD's report, please report the issue through HUD's Ask a Question (AAQ) portal.")
          ),
          
          accordion_panel(
            paste0("FY", FY, " HUD CoC Program NOFO Opportunities"),
            HTML("
              <p>Select the project and population types your CoC has prioritized for <strong>CoC Bonus/Reallocation</strong> and <strong>DV Bonus</strong> funding. Because NOFO opportunities might be updated, CoCs should ensure that the project types and populations they have selected are eligible in the current CoC NOFO.</p>
              
              <p>New and Expansion projects with project types and populations that match the selected categories will be indicated on the <strong>Ranking</strong> page with the respective color of that funding category, so long as there is funding left in the relevant bucket after higher ranked projects are selected. All projects that match project type and populations for either the CoC or DV Bonus will have that information indicated in the ranked list on the <strong>Ranking</strong> page. CoCs can use that information to help understand how HUD selection processes in the NOFO will be used to select projects.</p>
            ")
          ),
          
          accordion_panel(
            "Funding Ceilings and Priorities by Project Type and Population",
            p("Specify your CoC’s system needs and the relative priority of different parts of your homeless system for purposes of the CoC Program application. For each project type/population combination, you can specify the relative priority, maximum number of beds (renewal and new combined), and/or maximum level of funding")
          )
        )
      ),
      "rating"                                           = list(
        HTML("
          <p>The Rating page provides two options to score projects:</p>
          
          <ul>
            <li><strong>In-App</strong>: Rate projects directly in the tool after selecting and customizing threshold and rating criteria</li>
            <li><strong>Alternative</strong>: Rate projects externally using a local approach and enter only the final final results in the tool </li>
          </ul>
          
          <p>The tool uses a two-step process for rating – a threshold requirements review and performance scoring. These steps can be performed at the same time or in phases. </p>
          
          <p>If using the In-App option, use the <strong>Generate Report Card</strong> button to download a blank template of your selected threshold and rating criteria so your CoC can use paper copies as part of its rating process, if desired. You can also download Report Cards for projects with completed ratings.</p>
        "),
        accordion(
          accordion_panel(
            "In-App Rating",
            HTML("
              <p>There are three main tabs under the In-App Rating option:</p>
        
              <ul>
                <li><strong>Customize Rating Criteria:</strong> Allows you to customize CoC-specified threshold requirements and rating factors for Renewal and New projects</li>
                <li><strong>Rate Renewal/Expansion Projects:</strong> Allows you to enter threshold reviews and performance scoring for Renewal/Expansion projects</li>
                <li><strong>Rate New Projects:</strong> Allows you to enter threshold reviews and performance scoring for New projects</li>
              </ul>
        
              <p>Because New projects do not have historical performance data to base ratings on, the tool provides different rating processes for Renewal/Expansion projects and for New projects.</p>
            "),
            
            accordion(
              accordion_panel(
                "Customize Rating Criteria",
                HTML("
                  <p>Before rating your projects, review the selections under the CoC Thresholds Requirements and Rating Factors subtabs. You can also customize thresholds and rating factors with local criteria.</p>
        
                  <p>The rating criteria you select and customize in this tab will appear in the <strong>Rate Renewal/Expansion Projects</strong> and <strong>Rate New Projects</strong> tabs.</p>
                ")
              ),
              
              accordion_panel(
                "Rate Renewal/Expansion Projects and Rate New Projects",
                HTML("
                  <p>These two tabs allow you to rate your projects against your selected rating criteria. Each of the Rate Projects tabs are divided into two sections:</p>
        
                  <ul>
                    <li><strong>Threshold Entry:</strong> Used to determine if the project meets specified HUD and CoC threshold requirements</li>
                    <li><strong>Rating Entry:</strong> Used to score projects on rating factors</li>
                  </ul>
        
                  <p>Use the <strong>Select Project</strong> drop-down list to pick the project you want to rate. Projects that have both the <strong>Threshold Complete</strong> and <strong>Rating Complete</strong> buttons toggled on appear with a checkmark in the <strong>Select Project</strong> drop-down list.</p>
                ")
              )
            )
          ),
          accordion_panel(
            "Alternative Rating",
            HTML("
                  <p>As an alternative to manual entry of a project's threshold review and rating scores, you can quickly enter threshold review results and a weighted rating score for each of your projects.</p>
                  
                  <p>Click the <strong>Download Template</strong> button to download an Alternative Rating template with a list of your projects. After thresholds and weighted scores have been completed for each project in the template, use the <strong>Import Rating</strong> button to upload your completed file into the tool.</p>
                ")
          )
        )
        
      ),
      # "rating-alternative"                               = rating_alt_instructions,
      # "rating-in_app"                                    = rating_in_app_instructions,
      # "rating-customize_criteria-renewal_rating_factors" = rating_in_app_instructions,
      # "rating-customize_criteria-new_rating_factors"     = rating_in_app_instructions,
      # "rating-renew-thresholds_entry"                    = rating_in_app_instructions,
      # "rating-renew-rating_scores_entry"                 = rating_in_app_instructions,
      # "rating-new-thresholds_entry"                      = rating_in_app_instructions,
      # "rating-new-rating_scores_entry"                   = rating_in_app_instructions,
      "ranking"                                          = HTML("
        <p>The <strong>Ranking</strong> page produces an initial ranked list of projects that reflects both the rating results from the Rating module and the funding priorities set by the NOFO and the CoC. This page includes summary tallies of dollar amounts within each funding category and a <strong>Funding Analysis Table</strong> that summarizes the units and funding allocated to each major project type and population group.</p>
        
        <p>To move individual projects up or down within the ranked list, click and hold the dotted icon at the far left of the project row and drag the project to the desired position. If you move a project to a new funding tier or out of the <strong>Projects Not Selected for Funding</strong> section, the funding summaries and <strong>Funding Analysis Table</strong> at the top will automatically adjust.</p>
        
        <p>If you want to reset the initial ranking values or change funding priorities and generate the initial ranking again, click the <strong>Regenerate Ranking</strong> button</p>
        
        <p>To adjust the allocation of projects in the <strong>Tier 1</strong> and <strong>Tier 2</strong> lists after changing funding levels for projects in the <strong>CoC Funding Recommendation</strong> column, click the <strong>Adjust Tiers after Funding Changes</strong> button. The tool will pull a new project into either <strong>Tier 1</strong> or <strong>Tier 2</strong> if there is any funding available in the Tier, potentially creating a straddle project.</p>
        
        <p>Clicking either the <strong>Regenerate Ranking</strong> or the <strong>Adjust Tiers after Funding Changes</strong> buttons could erase any changes you have made directly to the ranked list. To save a copy of your adjusted rankings, click the <strong>Export Ranking</strong> button to download the ranking list.</p>
        
        <p>The exported ranking list can be used to communicate recommended ranking results to applicants and CoC partners. The download can also be used to prepare annual CoC Project application materials for HUD.</p>
      ")
    )
    
    help_wrapper <- function(tab, instructions) {
      tagList(
        h4(
          stringr::str_to_title(paste0(tab, " Instructions")),
          div(
            style = "position: absolute; top: 10px; right: 20px;",
            actionButton(ns("close_help"), "X", class = "btn-danger btn-sm")
          )
        ),
        hr(),
        instructions
      )
    }
    
    observeEvent(user_coc$auth, {
      req(user_coc$auth)
      shinyjs::show("help_sidebar")
    })
    
    observeEvent(input$toggle_help, {
      shinyjs::toggleClass(id = "help_sidebar", class = "open")
    })
    
    # 2. Close via X button inside the sidebar
    observeEvent(input$close_help, {
      shinyjs::removeClass(id = "help_sidebar", class = "open")
    })
    
    output$dynamic_help <- renderUI({
      req(nav_control())
      help_wrapper(nav_control(), help_texts[[help_id()]])
    })
    
    
    # Force collapse sidebar when tab changes ---
    # This ensures that even if they had help open on Tab A, 
    # it's gone when they click Tab B.
    observeEvent(nav_control(), {
      help_id(nav_control())
      shinyjs::removeClass(id = "help_sidebar", class = "open")
    }, ignoreInit = TRUE) 
    
    return(help_id)
  })
}
    
    