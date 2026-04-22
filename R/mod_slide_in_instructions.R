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
      "dashboard"                                        = tagList(p("A paragraph"), p("Another paragraph")),
      "inventory"                                        = HTML("<p>Some text</p><ul>Followed by a bullet list<li>item 1</li><li>Item 2</li></ul>"),
      "funding_priorities"                               = "Funding priority instructions",
      "rating"                                           = "Select a method to begin.",
      "rating-alternative"                               = "Upload your external scores...",
      "rating-customize_criteria-coc_thresholds"         = tagList(h5("Customize CoC Thresholds"), p("Customize CoC threshold criteria")),
      "rating-customize_criteria-renewal_rating_factors" = tagList(h5("Customize Renewal/Expansion Project Rating Factors"), p("Customize renewal/expansion project rating factors")),
      "rating-customize_criteria-new_rating_factors"     = tagList(h5("Customize New Project Rating Factors"), p("Customize new project rating factors")),
      "rating-renew-thresholds_entry"                    = tagList(h5("Renewal/Expansion Project Thresholds"), p("Check threshold eligibility for renewals.")),
      "rating-renew-rating_scores_entry"                 = tagList(h5("Renewal/Expansion Project Scoring"), p("Enter raw scores for renewal projects.")),
      "rating-new-thresholds_entry"                      = tagList(h5("New Project Thresholds"), p("Check threshold eligibility for new projects.")),
      "rating-new-rating_scores_entry"                   = tagList(h5("New Project Scoring"), p("Enter raw scores for new projects.")),
      "ranking"                                          = "Ranking instructions"
    )
    
    help_wrapper <- function(tab, instructions) {
      tagList(
        h4(
          stringr::str_to_title(paste0(tab, " Instructions")),
          div(
            style = "position: absolute; top: 10px; right: 20px;",
            actionButton("close_help", "X", class = "btn-danger btn-sm")
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
    
    