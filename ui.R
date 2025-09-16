page_navbar(
  title = "CoC Project Rating and Ranking Tool",
  id = "nav",
  
  header = tagList(
    ## css, idle management, and dimension management --------
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    ## Enable shinyjs -----
    shinyjs::useShinyjs(),
    disconnectMessage(
      text = str_squish(
        "HORRT has crashed. Please submit an issue on GitHub and note the
          date and time (including timezone) in order to help the team diagnose the issue."
      ),
      overlayColour = '#F5F5F5',
      refresh = ""
    )
  ),

  nav_panel(
    title = 'About',
    value = 'about',
     ## log in button
     tags$a(id = "login_link", "Log in", class = 'btn btn-primary', href = aws_auth_redirect),
     ## create account button
     tags$a(id = "signup_link", "Create Account", class = "btn btn-primary", href = aws_auth_signup),

    h4('About the Tool'),
    HTML("<p>HUD is providing this Rating and Ranking Tool to help CoCs design and implement a comprehensive annual CoC competition application review process. It has several customization features so you can choose the rating factors that are most relevant to your CoC and the priorities your CoC has adopted to inform system (re)design.</p>
      <p>DISCLAIMER: HUD is explicitly stating that use of this tool is optional, is not being promoted over other tools CoCs currently use, and does not guarantee:</p>
     <ul>
     <li>additional points in the Fiscal Year (FY) 2024 Continuum of Care Program (CoC) Competition;</li>
     <li>CoC applications will be consistent with all NOFO requirements; and</li>
     <li>HUD will award CoCs with full points or funding.</li>
     </ul>
     <p>The tool provides a strong framework for implementing a data-driven rating process and a ranking process informed by system priorities and capacity analysis (if available) and it satisfies the objective criteria requirement in the FY 2024 CoC Program NOFO.  HUD strongly encourages CoCs to read the CoC Program NOFO carefully to determine if there are new opportunities, priorities, or expectations that your CoC might need to assess outside this tool.  The Priority Listing is the official project ranking record for the CoC Program NOFO.  HUD is not requiring CoCs to use this tool, nor is it preferred over other rating tools or processes, use of the tool does not guarantee additional points on the CoC Program application. HUD has made this tool available to CoCs for use in their year-round NOFO planning process.  Feedback on the tool is welcome.</p>"
    )
   
    
  ),
  
  #mod_requests_ui("requests"),
  nav_panel(title = 'My Dashboard', value = 'dashboard',
            mod_coc_selection_ui("coc_selection"),
            mod_requests_ui("requests")
            ),
  #mod_coc_selection_ui("coc_selection"),
  mod_inventory_ui("inventory"),
  mod_rating_criteria_ui("rating_criteria"),
  mod_renewal_rating_ui("renewal_rating"),
  mod_new_rating_ui("new_rating"),
  mod_alternative_rating_ui("alternative_rating"),
  mod_funding_priorities_ui("funding_priorities"),
  mod_final_review_ui("final_review"),
  mod_ranking_ui("ranking"),
  mod_account_ui("account")

)