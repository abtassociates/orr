mod_account_ui <- function(id){
  ns <- NS(id)
  
  ## dropdown menu for managing account - requests, instances, log out
  nav_menu(
    title = "Manage Account",
    icon = icon('user'),
    value = id,
    nav_panel( 
      title = 'Request Access',
      mod_requests_ui('requests')
    ),
    nav_panel(
      title = 'View Instances'
    ),
    ## link to log out of application
    nav_item(
      value = "sign_out",
      tags$a("Log Out", id = "submit_sign_out", href = aws_auth_logout)
    )
  )
}

mod_account_server <- function(id) {
  moduleServer(id, function(input, output, session) {
  })

}
