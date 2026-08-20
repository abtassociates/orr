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
      "dashboard"                                        = list(title = "Dashboard", content = list(
        p('The My Dashboard page displays your Rating and Ranking tool versions and version access requests'),
        accordion(
          id = "my_accordion", # Optional: used to read the open state on the server
          open = FALSE,         # Set to FALSE to collapse everything by default
          
          accordion_panel(
            "Versions",
            p('To create a new version of the tool, select the "Create New Version" button, then choose your Continuum of Care (CoC) and import your most recent HIC to begin.'),
            HTML('<p>To request access to a version created by someone else for your CoC, click the <strong>Request Access to an Existing Version</strong> button, select the CoC, and send a request to the original version creator. Multiple users can work together on the same tool at the same time. You will be able to see if they are on the same page as you at the top of the tool.</p>'),
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
      )),
        
      "inventory" = list(title = "Review Projects", content = HTML("
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
      )),
      
      "funding_priorities" = list(title = "Funding Priorities", content = list(
        HTML("<p>The <strong>Funding Ceilings + Priorities</strong> page allows you to specify system-wide funding priorities for your CoC's annual funding process.</p>"),
        accordion(
          accordion_panel(
            "General Funding Information",
            HTML("<p>Your CoC's <strong>Annual Renewal Demand (ARD)</strong> amount is based on HUD's Estimated Annual Renewal Demand Report. If the numbers do not match HUD's report, please report the issue through HUD's Ask a Question (AAQ) portal.</p>")
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
      )),
      "rating"                                           = list(title = "Rating", content = list(
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
        
      )),
      # "rating-alternative"                               = rating_alt_instructions,
      # "rating-in_app"                                    = rating_in_app_instructions,
      # "rating-customize_criteria-renewal_rating_factors" = rating_in_app_instructions,
      # "rating-customize_criteria-new_rating_factors"     = rating_in_app_instructions,
      # "rating-renew-thresholds_entry"                    = rating_in_app_instructions,
      # "rating-renew-rating_scores_entry"                 = rating_in_app_instructions,
      # "rating-new-thresholds_entry"                      = rating_in_app_instructions,
      # "rating-new-rating_scores_entry"                   = rating_in_app_instructions,
      "ranking"                                          = list(title = "Ranking", content = list(
      HTML("
        <p>The <strong>Ranking</strong> page produces an initial ranked list of projects that reflects both the rating results from the Rating module and the funding priorities set by the NOFO and the CoC. This page includes summary tallies of dollar amounts within each funding category and a <strong>Funding Analysis Table</strong> that summarizes the units and funding allocated to each major project type and population group.</p>
        
        <p>To move individual projects up or down within the ranked list, click and hold the dotted icon at the far left of the project row and drag the project to the desired position. If you move a project to a new funding tier or out of the <strong>Projects Not Selected for Funding</strong> section, the funding summaries and <strong>Funding Analysis Table</strong> at the top will automatically adjust.</p>
        
        <p>If you want to reset the initial ranking values or change funding priorities and generate the initial ranking again, click the <strong>Regenerate Ranking</strong> button</p>
        
        <p>To adjust the allocation of projects in the <strong>Tier 1</strong> and <strong>Tier 2</strong> lists after changing funding levels for projects in the <strong>CoC Funding Recommendation</strong> column, click the <strong>Adjust Tiers after Funding Changes</strong> button. The tool will pull a new project into either <strong>Tier 1</strong> or <strong>Tier 2</strong> if there is any funding available in the Tier, potentially creating a straddle project.</p>
        
        <p>Clicking either the <strong>Regenerate Ranking</strong> or the <strong>Adjust Tiers after Funding Changes</strong> buttons could erase any changes you have made directly to the ranked list. To save a copy of your adjusted rankings, click the <strong>Export Ranking</strong> button to download the ranking list.</p>
        
        <p>The exported ranking list can be used to communicate recommended ranking results to applicants and CoC partners. The download can also be used to prepare annual CoC Project application materials for HUD.</p>
      "),
      accordion(
        accordion_panel(
          "Straddle Projects",
          HTML("
          <p>Projects may exceed the amount of adjusted ARD available in Tier 1 
          and 'straddle' the Tier 1/Tier 2 line. DV Bonus projects in Tier 1 that
          are awarded funding may result in other projects moving up into Tier 1
          and possibly straddling the Tier 1/Tier 2 line. CoCs should carefully
          review this and the NOFO treatment of scoring and awards of straddle 
          projects. Funding may be adjusted, or projects moved to reduce or 
          eliminate the straddle. Projects may also exceed the Tier 2 line which 
          means the projects exceed the funding available to the CoC.</p>
            ")
        ),
        accordion_panel(
          "Determining Project Priority",
          HTML("The following logic is used to determine the rank order of 
               projects based on inventory entered on the <strong>Review Projects</strong> page 
               and the priorities set on the <strong>Funding Ceilings + Priorities</strong> page."),
          br(),
          br(),
          table(
            class = "table table-bordered",
            
            thead(
              tr(
                th("Ratio of Beds Within a Project"),
                th("How Priority is Determined")
              )
            ),
            
            tbody(
              
              tr(
                td(
                  colspan = 2,
                  em(strong(
                    "If a Project has any beds dedicated to a subpopulation..."
                  ))
                )
              ),
              
              tr(
                td(
                  ol(
                    li("Does the project have DV beds?")
                  )
                ),
                td(
                  ul(
                    li(
                      "The project is ranked based on the Priority (e.g. DV Fam, DV Ind)."
                    )
                  )
                )
              ),
              
              tr(
                td(
                  ol(
                    start = 2,
                    li(
                      "Does the project have at least 50% of its total beds dedicated to ",
                      "CH Fam, CH Ind, Vet Ind OR Parenting Youth?"
                    )
                  )
                ),
                td(
                  ul(
                    li(
                      "The project is ranked based on that subpopulation's priority."
                    ),
                    li(
                      "If multiple subpopulations meet the 50+% threshold, ranking is based on ",
                      "the highest ranked of the subpopulations."
                    )
                  )
                )
              ),
              
              tr(
                td(
                  ol(
                    start = 3,
                    li(
                      "Even though no single subpopulation meets the 50% threshold, is the sum ",
                      "of dedicated beds 50% or greater?"
                    )
                  )
                ),
                td(
                  ul(
                    li(
                      "Both subpopulation and population priorities are ignored for ranking ",
                      "purposes, and the project is ranked with other projects that meet ",
                      "unspecified priorities."
                    )
                  )
                )
              ),
              
              tr(
                td(
                  ol(
                    start = 4,
                    li("Is the sum of dedicated beds less than 50%?")
                  )
                ),
                td(
                  ul(
                    li(
                      "Subpopulation priorities are ignored for ranking purposes, and the ",
                      "project is ranked solely based on the overarching population priorities."
                    )
                  )
                )
              ),
              
              tr(
                td(
                  colspan = 2,
                  em(strong(
                    "If a Project doesn't have any beds dedicated to a subpopulation..."
                  ))
                )
              ),
              
              tr(
                td(
                  ol(
                    li(
                      "Does the project target > 50% of its beds to Families OR Individuals?"
                    )
                  )
                ),
                td(
                  "The project is ranked based on the Priority and Funding Ceilings of the ",
                  "Majority Populations."
                )
              ),
              
              tr(
                td(
                  ol(
                    start = 2,
                    li(
                      "Does the project evenly target two populations (50%/50%)?"
                    )
                  )
                ),
                td(
                  "Ranking is based on the Priority and Funding Ceilings of the Highest ",
                  "Ranked Populations (Fam, Ind)."
                )
              ),
              
              tr(
                td(
                  colspan = 2,
                  em(strong(
                    "If a Project is Eligible for CoC Bonus or DV Funding..."
                  ))
                )
              ),
              
              tr(
                td(
                  ol(
                    li(
                      "Is there bonus or reallocated funding remaining and is the project ",
                      "selected for CoC Bonus/Reallocation consideration in the NOFO ",
                      "Opportunities section?"
                    )
                  )
                ),
                td(
                  ul(
                    li(
                      "The project is highlighted in the list on the Ranking page to indicate ",
                      "it is a CoC Bonus project."
                    )
                  )
                )
              ),
              
              tr(
                td(
                  ol(
                    start = 2,
                    li(
                      "Is there DV Bonus funding remaining and is the project a type selected ",
                      "for DV Bonus/Reallocation consideration in the NOFO Opportunities section?"
                    )
                  )
                ),
                td(
                  ul(
                    li(
                      "The project is highlighted in the list on the Ranking page to indicate ",
                      "it is a DV Bonus project."
                    )
                  )
                )
              )
            )
          )
        ),
        
        
        bslib::accordion_panel(
          "Allocating Beds",
          
          HTML(
            'The following logic is used to allocate beds and funding to the funding targets
      indicated on the <strong>Funding Ceilings + Priorities</strong> page.'
          ),
          
          br(),
          br(),
          table(
            class = "table table-bordered",
            
            thead(
              tr(
                th("Ratio of Beds Within a Project"),
                th("How Subpopulations are Analyzed"),
                th(
                  HTML('How <strong>All Families</strong> and <strong>All Individuals</strong> ',
                       'Bed/$ Counts are Allocated')
                ),
                th("How funding caps are allocated"),
                th("Notes")
              )
            ),
            
            tbody(
              
              tr(
                td(
                  colspan = 5,
                  em(strong(
                    "If a Project does not have any beds dedicated to a subpopulation..."
                  ))
                )
              ),
              
              tr(
                td(),
                td("N/A"),
                td("All beds are counted for their assigned population group."),
                td(
                  "All $ are counted for their assigned population group, pro-rated by beds."
                ),
                td()
              ),
              
              tr(
                td(
                  colspan = 5,
                  em(strong(
                    "If a Project has any beds dedicated to a subpopulation..."
                  ))
                )
              ),
              
              tr(
                td(
                  ol(
                    li("Does the project have DV beds?")
                  )
                ),
                td(
                  "Beds are allocated to the DV need (e.g., DV Fam, DV Ind)."
                ),
                td(
                  "Beds are not allocated to All Fam need or All Ind need. ",
                  "The beds are able to meet the needs of non-DV Fam or non-DV Ind."
                ),
                td(
                  "$ are allocated to the DV need (e.g., DV Fam, DV Ind), prorated by beds."
                ),
                td(
                  "If DV is selected, the populations all of the Population Beds ",
                  "(e.g., Fam or Ind) to DV."
                )
              ),
              
              tr(
                td(
                  ol(
                    start = 2,
                    li(
                      "Does the project have at least 50% of its beds dedicated to ",
                      "CH Fam, CH Ind, Vet Fam, Vet Ind, OR Parenting Youth?"
                    )
                  )
                ),
                td(
                  "Subpopulation beds are counted in their specific subpopulation category."
                ),
                td(
                  "After subtracting dedicated beds, the remaining or Ind beds are counted ",
                  "within their specific Population category."
                ),
                td(
                  "All subpopulation $ is prorated based on the beds and their specific ",
                  "subpopulation category."
                ),
                td(
                  "The tool does not attempt to reconcile; therefore a project that is 100% ",
                  "dedicated to CH and Vets will be counted as meeting both the Vet and CH ",
                  "criteria. You may need to manually adjust ranking if projects with combined ",
                  "eligibility criteria do not adequately meet identified unmet needs."
                )
              ),
              
              tr(
                td(
                  ol(
                    start = 3,
                    li(
                      "Even if no single subpopulation meets the 50% threshold, is the sum ",
                      "of dedicated beds 50% or greater of the total project beds?"
                    ),
                    li("Is the sum of dedicated beds less than 50%?")
                  )
                ),
                td(
                  "E.g., if a 100 bed project for Individuals has 25 beds for CH and 25 beds ",
                  "for Vets, those beds would count toward the CH and Vets."
                ),
                td(
                  "E.g., if a 100 bed project for Individuals has 25 beds for CH and 25 beds ",
                  "for Vets, 25% of the beds would be counted toward CH, 25% for Vets, and ",
                  "50% for All Ind."
                ),
                td(
                  "E.g., if a 100 bed project for Individuals has 25 beds for CH and 25 beds ",
                  "for Vets, 50% of the beds would be counted toward CH, 25% for Vets, and ",
                  "50% for All Ind."
                ),
                td()
              )
            )
          )
        )
      )
    ))
    )
    
    help_wrapper <- function(tab, instructions) {
      tagList(
        h4(
          stringr::str_to_title(paste0(help_texts[[tab]]$title, " Instructions")),
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
      help_wrapper(nav_control(), help_texts[[help_id()]]$content)
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
    
    