mod_account_ui <- function(id){
  ns <- NS(id)
  
  ## dropdown menu for managing account - requests, versions, log out
  nav_menu(
    title = "Account",
    icon = icon('user'),
    value = id,
    nav_item(
      value = "user_details",
      textOutput(ns('username'))
    ),
    ## link to log out of application
    nav_item(
      value = "sign_out",
      tags$a(
        "Log Out", 
        id = "submit_sign_out", 
        href = aws_auth_logout,
        onclick = "clear_cookie()"
      )
    )
  )
}

mod_account_server <- function(id, nav_control, user_coc, parent_session, help_id) {
  moduleServer(id, function(input, output, session) {
    
    output$username <- renderText({
      req(user_coc$auth)
      paste0('Username: ', user_coc$username)
    })
    
  })
}
