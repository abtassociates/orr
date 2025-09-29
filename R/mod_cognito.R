mod_cognito_ui <- function(id){
  useShinyjs()
  ns <- NS(id)
 
  tags$div(
    id = 'cognito_modal',
    modalDialog(
      title = 'Welcome to ORR!',
    
      ## welcome text
      div(
        id = "login_welcome_text",
        p("HUD is providing this Online Rating and Ranking Tool to help CoCs design and implement a comprehensive annual CoC competition application review process."),
        p("To use this tool, you must make a (free) account through AWS Cognito. Use the buttons below to create an account or log in if you have already made an account. Once you are logged in, it will return you to this page to access the tool."),
      ),
    
      easyClose = FALSE,
      
      footer = tagList(
        # ## log in button
        # tags$a(id = "login_link", "Log in", class = 'btn btn-primary', href = aws_auth_redirect),
        # ## create account button
        # tags$a(id = "signup_link", "Create Account", class = "btn btn-primary", href = aws_auth_signup)
      )
    )
  )
}
