mod_account_ui <- function(id){
  ns <- NS(id)
  
  ## dropdown menu for managing account - requests, versions, log out
  nav_menu(
    title = "Account",
    icon = icon('user'),
    value = id,
    # nav_panel( 
    #   title = 'Request Access',
    #   mod_requests_ui('requests')
    # ),
    # nav_panel(
    #   title = 'View Versions'
    # ),
    # nav_item(
    #   value = "sign_in",
    #   tags$a("Log in", id = "submit_sign_in", href = aws_auth_redirect)
    # ),
    # nav_item(
    #   value = "sign_up",
    #   tags$a("Create Account", id = "submit_sign_up", href = aws_auth_signup)
    # ),
    tags$script(HTML(sprintf("
    document.addEventListener('click', function(e) {
      var el = e.target.closest('#submit_sign_out');
      if (!el) return;
      
      e.preventDefault(); // stops the href navigation
      
      hide_disconnect();
      
      window.location.href = '%s';
    });", aws_auth_logout))),
    nav_item(
      value = "user_details",
      textOutput(ns('username'))
    ),
    ## link to log out of application
    nav_item(
      value = "sign_out",
      tags$a("Log Out", id = "submit_sign_out", href = aws_auth_logout)
    )
  )
}

mod_account_server <- function(id, nav_control, user_coc, parent_session) {
  moduleServer(id, function(input, output, session) {
    
    output$username <- renderText({
      req(user_coc$auth)
      paste0('Username: ', user_coc$username)
    })
    
  })
}
