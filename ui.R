page_navbar(
  title = "CoC Project Rating and Ranking Tool",
  id = "nav",
  fillable = FALSE,
  window_title = 'ORR',
  
  theme = orr_bslib_theme,
  navbar_options = orr_navbar_options,
  
  includeCSS(here("www/custom.css")),
  
  header = tagList(
    ## css, idle management, and dimension management --------
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tags$style(HTML("
    /* Change the background color of the selected row */
    table.dataTable tbody tr.selected>* {
      box-shadow: inset 0 0 0 9999px #357DAD !important;
      color: white;
    }
    table.dataTable.display > tbody > tr.selected:hover>* {
      box-shadow: inset 0 0 0 9999px #357DAD !important;
      color: white;
    }
  ")),
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
    icon = icon("info"),
    
    card(id = 'about_card',fill = FALSE,
         card_header(h4('Welcome to the CoC Project Rating and Ranking Tool!')),
         card_body(
          fillable = FALSE,
          fill = TRUE,
        
        # HTML("<p>HUD is providing this Rating and Ranking Tool to help CoCs design and implement a comprehensive annual CoC competition application review process. It has several customization features so you can choose the rating factors that are most relevant to your CoC and the priorities your CoC has adopted to inform system (re)design.</p>
        #   <p>DISCLAIMER: HUD is explicitly stating that use of this tool is optional, is not being promoted over other tools CoCs currently use, and does not guarantee:</p>
        #  <ul>
        #  <li>additional points in the Fiscal Year (FY) 2026 Continuum of Care Program (CoC) Competition;</li>
        #  <li>CoC applications will be consistent with all NOFO requirements; and</li>
        #  <li>HUD will award CoCs with full points or funding.</li>
        #  </ul>
        #  <p>The tool provides a strong framework for implementing a data-driven rating process and a ranking process informed by system priorities and capacity analysis (if available) and it satisfies the objective criteria requirement in the FY 2026 CoC Program NOFO.  HUD strongly encourages CoCs to read the CoC Program NOFO carefully to determine if there are new opportunities, priorities, or expectations that your CoC might need to assess outside this tool.  The Priority Listing is the official project ranking record for the CoC Program NOFO.  HUD is not requiring CoCs to use this tool, nor is it preferred over other rating tools or processes, use of the tool does not guarantee additional points on the CoC Program application. HUD has made this tool available to CoCs for use in their year-round NOFO planning process.  Feedback on the tool is welcome.</p>"
        # ),
        
        HTML("<p>
             The CoC Project Online Rating and Ranking (ORR) Tool is an optional, customizable tool provided by HUD that can be used by Continuums of Care (CoCs) to evaluate project performance and rank projects based on priorities, thresholds, and rating factors that are relevant to the CoC. The tool provides a strong framework for implementing a data-driven rating and ranking process informed by system priorities and capacity analysis (if available), and it satisfies the objective criteria requirement in the CoC Program Notice of Funding Opportunity (NOFO).</p>
             <h5>Disclaimer</h5>
             <p>HUD is explicitly stating that use of this tool is optional. HUD is not promoting this tool over other tools CoCs currently use. Additionally, using this tool does not guarantee:
             <ul>
             <li>Additional points in the annual Continuum of Care Program (CoC) Competition</li>
             <li>CoC applications will be consistent with all NOFO requirements</li>
             <li>HUD will award CoCs with full points or funding</li>
             </ul>
             <p>HUD strongly encourages CoCs to read the CoC Program NOFO carefully to determine if there are new opportunities, priorities, or expectations that your CoC might need to assess outside this tool. HUD has made this tool available to CoCs for use in their year-round NOFO planning process. Feedback on the tool is welcome.
             </p>"),
        
        ## log in button
        tags$a(id = "login_link", "Log in", class = 'btn btn-primary', href = aws_auth_redirect),
        ## create account button
        tags$a(id = "signup_link", "Create Account", class = "btn btn-primary", href = aws_auth_signup),
   
      )
    )
  )
)