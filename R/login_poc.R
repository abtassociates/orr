# Load required libraries
library(shiny)
library(shinymanager)
library(DBI)
library(RPostgres)
library(digest)
library(DT)

# Database configuration
DB_CONFIG <- list(
  host = Sys.getenv("AWS_RDS_HOST"),
  port = as.integer(Sys.getenv("AWS_RDS_PORT", "3306")),
  dbname = Sys.getenv("AWS_RDS_DBNAME"),
  username = Sys.getenv("AWS_RDS_USERNAME"),
  password = Sys.getenv("AWS_RDS_PASSWORD")
)

# Database connection function
get_db_connection <- function() {
  dbConnect(
    RPostgreSQL::PostgreSQL(),
    host = DB_CONFIG$host,
    port = DB_CONFIG$port,
    dbname = DB_CONFIG$dbname,
    user = DB_CONFIG$username,
    password = DB_CONFIG$password,
    sslmode = "require"
  )
}

# Initialize database tables
initialize_db <- function() {
  con <- get_db_connection()
  
  # Create users table if it doesn't exist
  query <- "
  CREATE TABLE IF NOT EXISTS users (
    id SERIAL PRIMARY KEY,
    user VARCHAR(50) UNIQUE NOT NULL,
    password VARCHAR(255) NOT NULL,
    admin BOOLEAN DEFAULT FALSE,
    start DATE,
    expire DATE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  )"
  
  dbExecute(con, query)
  
  # Create default admin user if no users exist
  user_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM users")$count
  if (user_count == 0) {
    admin_password <- digest("admin123", algo = "sha256")
    dbExecute(con, "INSERT INTO users (user, password, admin, start, expire) VALUES ($1, $2, TRUE, CURRENT_DATE, CURRENT_DATE + INTERVAL '1 year')", 
              list("admin", admin_password))
  }
  
  dbDisconnect(con)
}

# Custom authentication function
auth_fun <- function(user, password) {
  con <- get_db_connection()
  
  # Hash the provided password
  hashed_password <- digest(password, algo = "sha256")
  
  # Query user from database
  query <- "SELECT * FROM users WHERE user = $1 AND password = $2"
  result <- dbGetQuery(con, query, list(user, hashed_password))
  
  dbDisconnect(con)
  
  if (nrow(result) > 0) {
    user_info <- result[1, ]
    
    # Check if account is expired
    if (!is.na(user_info$expire) && as.Date(user_info$expire) < Sys.Date()) {
      return(FALSE)
    }
    
    # Check if account is active
    if (!is.na(user_info$start) && as.Date(user_info$start) > Sys.Date()) {
      return(FALSE)
    }
    
    return(TRUE)
  }
  
  return(FALSE)
}

# Function to create new user
create_user <- function(username, password, admin = FALSE) {
  con <- get_db_connection()
  
  # Check if user already exists
  existing_user <- dbGetQuery(con, "SELECT COUNT(*) as count FROM users WHERE user = $1", list(username))$count
  
  if (existing_user > 0) {
    dbDisconnect(con)
    return(list(success = FALSE, message = "Username already exists"))
  }
  
  # Hash password
  hashed_password <- digest(password, algo = "sha256")
  
  # Insert new user
  tryCatch({
    dbExecute(con, "INSERT INTO users (user, password, admin, start, expire) VALUES ($1, $2, $3, CURRENT_DATE, CURRENT_DATE + INTERVAL '1 year')", 
              list(username, hashed_password, admin))
    dbDisconnect(con)
    return(list(success = TRUE, message = "User created successfully"))
  }, error = function(e) {
    dbDisconnect(con)
    return(list(success = FALSE, message = paste("Error creating user:", e$message)))
  })
}

# Initialize database
initialize_db()

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .login-container {
        max-width: 400px;
        margin: 50px auto;
        padding: 20px;
        border: 1px solid #ddd;
        border-radius: 5px;
        box-shadow: 0 2px 5px rgba(0,0,0,0.1);
      }
      .signup-form {
        margin-top: 20px;
        padding: 20px;
        background-color: #f9f9f9;
        border-radius: 5px;
      }
    "))
  ),
  
  # Custom login UI
  div(id = "login-container", class = "login-container",
      h2("Login", style = "text-align: center;"),
      
      textInput("username", "Username:", placeholder = "Enter username"),
      passwordInput("password", "Password:", placeholder = "Enter password"),
      
      div(style = "text-align: center; margin: 20px 0;",
          actionButton("login_btn", "Login", class = "btn btn-primary", style = "width: 100%;")
      ),
      
      hr(),
      
      div(class = "signup-form",
          h4("Create New Account"),
          textInput("new_username", "New Username:", placeholder = "Choose username"),
          passwordInput("new_password", "New Password:", placeholder = "Choose password"),
          passwordInput("confirm_password", "Confirm Password:", placeholder = "Confirm password"),
          
          div(style = "text-align: center; margin: 15px 0;",
              actionButton("signup_btn", "Create Account", class = "btn btn-success", style = "width: 100%;")
          )
      ),
      
      # Message area
      div(id = "message_area", style = "margin-top: 15px;")
  ),
  
  # Main application content (hidden initially)
  div(id = "main-content", style = "display: none;",
      navbarPage("My Shiny App",
                 tabPanel("Dashboard",
                          fluidRow(
                            column(12,
                                   h1("Welcome to Your Dashboard!"),
                                   br(),
                                   div(style = "text-align: right;",
                                       actionButton("logout_btn", "Logout", class = "btn btn-danger")
                                   ),
                                   br(),
                                   wellPanel(
                                     h3("Sample Content"),
                                     p("This is your main application content. You can add any Shiny components here."),
                                     
                                     # Sample interactive content
                                     h4("Sample Data Table"),
                                     DT::dataTableOutput("sample_table")
                                   )
                            )
                          )
                 ),
                 
                 tabPanel("Profile",
                          fluidRow(
                            column(12,
                                   h2("User Profile"),
                                   br(),
                                   div(id = "user_info_area")
                            )
                          )
                 )
      )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    authenticated = FALSE,
    current_user = NULL
  )
  
  # Sample data for demonstration
  output$sample_table <- DT::renderDataTable({
    if (values$authenticated) {
      data.frame(
        Name = c("John Doe", "Jane Smith", "Bob Johnson"),
        Age = c(30, 25, 35),
        City = c("New York", "Los Angeles", "Chicago"),
        stringsAsFactors = FALSE
      )
    }
  })
  
  # Login logic
  observeEvent(input$login_btn, {
    req(input$username, input$password)
    
    if (auth_fun(input$username, input$password)) {
      values$authenticated <- TRUE
      values$current_user <- input$username
      
      # Hide login form and show main content
      runjs("document.getElementById('login-container').style.display = 'none';")
      runjs("document.getElementById('main-content').style.display = 'block';")
      
      # Clear login inputs
      updateTextInput(session, "username", value = "")
      updateTextInput(session, "password", value = "")
      
      showNotification("Login successful!", type = "success")
    } else {
      insertUI(
        selector = "#message_area",
        ui = div(class = "alert alert-danger", "Invalid username or password!"),
        immediate = TRUE
      )
    }
  })
  
  # Signup logic
  observeEvent(input$signup_btn, {
    req(input$new_username, input$new_password, input$confirm_password)
    
    # Clear previous messages
    removeUI(selector = "#message_area > *")
    
    # Validate passwords match
    if (input$new_password != input$confirm_password) {
      insertUI(
        selector = "#message_area",
        ui = div(class = "alert alert-danger", "Passwords do not match!"),
        immediate = TRUE
      )
      return()
    }
    
    # Validate password length
    if (nchar(input$new_password) < 6) {
      insertUI(
        selector = "#message_area",
        ui = div(class = "alert alert-danger", "Password must be at least 6 characters long!"),
        immediate = TRUE
      )
      return()
    }
    
    # Validate username
    if (nchar(input$new_username) < 3) {
      insertUI(
        selector = "#message_area",
        ui = div(class = "alert alert-danger", "Username must be at least 3 characters long!"),
        immediate = TRUE
      )
      return()
    }
    
    # Create user
    result <- create_user(input$new_username, input$new_password)
    
    if (result$success) {
      insertUI(
        selector = "#message_area",
        ui = div(class = "alert alert-success", result$message),
        immediate = TRUE
      )
      
      # Clear signup form
      updateTextInput(session, "new_username", value = "")
      updateTextInput(session, "new_password", value = "")
      updateTextInput(session, "confirm_password", value = "")
    } else {
      insertUI(
        selector = "#message_area",
        ui = div(class = "alert alert-danger", result$message),
        immediate = TRUE
      )
    }
  })
  
  # Logout logic
  observeEvent(input$logout_btn, {
    values$authenticated <- FALSE
    values$current_user <- NULL
    
    # Show login form and hide main content
    runjs("document.getElementById('login-container').style.display = 'block';")
    runjs("document.getElementById('main-content').style.display = 'none';")
    
    # Clear any messages
    removeUI(selector = "#message_area > *")
    
    showNotification("Logged out successfully!", type = "message")
  })
  
  # User info display
  output$user_info <- renderUI({
    if (values$authenticated) {
      con <- get_db_connection()
      user_info <- dbGetQuery(con, "SELECT * FROM users WHERE user = $1", list(values$current_user))
      dbDisconnect(con)
      
      if (nrow(user_info) > 0) {
        user_data <- user_info[1, ]
        div(
          h4(paste("Welcome,", user_data$user)),
          p(paste("Account created:", user_data$created_at)),
          p(paste("Account expires:", user_data$expire)),
          p(paste("Admin privileges:", ifelse(user_data$admin, "Yes", "No")))
        )
      }
    }
  })
  
  # Insert user info in profile tab
  observe({
    if (values$authenticated) {
      insertUI(
        selector = "#user_info_area",
        ui = uiOutput("user_info"),
        immediate = TRUE
      )
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)