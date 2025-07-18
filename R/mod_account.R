mod_account_ui <- function(id){
  ns <- NS(id)
  
  nav_menu(
    title = "Manage Account",
    icon = icon('user'),
    value = id,
    nav_panel( 
      title = 'Request Access'
    ),
    nav_panel(
      title = 'View Instances'
    ),
    nav_panel(
      title = 'Log Out'
    )
  )
}

mod_account_server <- function(id) {
  moduleServer(id, function(input, output, session) {
  })
}
    