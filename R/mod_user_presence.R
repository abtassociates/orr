# UI
mod_user_presence_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("banner"))
}

# SERVER
# Arguments:
# - record_id: reactive returning the ID of the CoC Version or Project
# - field: reactive returning the specific input name or "all"
# - active: reactive (e.g., input$nav == "rating_tab")
mod_user_presence_server <- function(id, user_coc, record_id, field = reactive("all"), active = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    
    # ---------------------------------------------------------
    # 1. THE HEARTBEAT (UPSERT)
    # ---------------------------------------------------------
    observe({
      invalidateLater(10000, session) # 10s pulse
      
      # Extract username from reactiveValues
      username <- user_coc$username
      
      # Only execute if form is active, user is logged in, and a record is selected
      req(active(), username, record_id())
      
      # Parameters must be in a list in the order of $1, $2, etc.
      params <- list(
        session$token,           # $1
        id,                      # $2 (The module namespace ID)
        username,                # $3
        as.character(record_id()),# $4 (Cast to char to handle both int/char IDs)
        field()                  # $5
      )
      
      db_execute(
        "INSERT INTO user_presence (session_id, context, user_id, record_id, field, last_seen)
         VALUES ($1, $2, $3, $4, $5, CURRENT_TIMESTAMP)
         ON CONFLICT (session_id, context)
         DO UPDATE SET
            user_id = EXCLUDED.user_id,
            record_id = EXCLUDED.record_id,
            field = EXCLUDED.field,
            last_seen = CURRENT_TIMESTAMP;",
        params = params
      )
    })
    
    # ---------------------------------------------------------
    # 2. POLLING (Who else is here?)
    # ---------------------------------------------------------
    active_others <- reactive({
      invalidateLater(10000, session)
      req(active(), record_id())
      
      # Dialect-agnostic threshold calculation
      # We calculate the cutoff in R to avoid INTERVAL (Postgres) vs datetime (SQLite) syntax
      threshold <- format(Sys.time() - 25, "%Y-%m-%d %H:%M:%S", tz = "UTC")
      
      params <- list(
        id,                      # $1
        username,
        as.character(record_id()),# $2
        field(),                 # $3
        session$token,           # $4
        threshold                # $5
      )
      
      res <- get_db_query(
        "SELECT DISTINCT user_id 
         FROM user_presence 
         WHERE context = $1 
           AND user_id != $2
           AND record_id = $3 
           AND field = $4
           AND session_id != $5
           AND last_seen > $6;",
        params = params
      )
      
      if(is.data.frame(res) && nrow(res) > 0) return(res$user_id)
      return(character(0))
    })
    
    # ---------------------------------------------------------
    # 3. UI RENDER
    # ---------------------------------------------------------
    output$banner <- renderUI({
      others <- active_others()
      req(length(others) > 0)
      
      div(class = "alert alert-warning", 
          style = "padding: 8px; margin-bottom: 10px; font-size: 0.9em; border-left: 5px solid #f0ad4e;",
          icon("user-friends"), 
          paste(" Presence Detected:", paste(others, collapse = ", "), "is also viewing this section."))
    })
    
    # ---------------------------------------------------------
    # 4. CLEANUP
    # ---------------------------------------------------------
    # Clear presence when user switches tabs (active becomes FALSE)
    observeEvent(active(), {
      if(!active()) {
        db_execute(
          "DELETE FROM user_presence WHERE session_id = $1 AND context = $2;",
          params = list(session$token, id)
        )
      }
    }, ignoreInit = TRUE)
    
    # Clear presence when session ends (tab closed)
    session$onSessionEnded(function() {
      db_execute(
        "DELETE FROM user_presence WHERE session_id = $1;",
        params = list(session$token)
      )
    })
  })
}