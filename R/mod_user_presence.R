# ==========================================
# UI
# ==========================================
mod_user_presence_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("banner"))
}

# ==========================================
# SERVER
# ==========================================
# Arguments:
# - id: Inherently tells us the context (where in the app the user is)
# - user_coc: REACTIVEVALUES returning the logged-in user's info
# - record_id: REACTIVE returning the ID of the record currently being viewed
# - field: REACTIVE returning the field being edited (Defaults to "all")
mod_user_presence_server <- function(id, user_coc, record_being_edited) {
  moduleServer(id, function(input, output, session) {
    # 1. THE HEARTBEAT (UPSERT)
    observe({
      invalidateLater(5000, session)
      
      # Extract username. Ensure it exists.
      username <- user_coc$username
      req(username, record_being_edited()$record_id)
      
      query <- glue::glue_sql("
        INSERT INTO user_presence (session_id, context, user_id, record_id, field, last_seen)
        VALUES ({session$token}, {id}, {username}, {record_being_edited()$record_id}, {record_being_edited()$field}, CURRENT_TIMESTAMP)
        ON CONFLICT (session_id, context)
        DO UPDATE SET
            user_id = EXCLUDED.user_id,
            record_id = EXCLUDED.record_id,
            field = EXCLUDED.field,
            last_seen = CURRENT_TIMESTAMP;
      ", .con = get_db_pool())
      
      db_execute(query)
    })
    
    # 2. PRESENCE POLLING
    active_others <- reactive({
      invalidateLater(5000, session)
      req(record_being_edited()$record_id)
      
      # Notice we use `id` here as the context filter
      query <- glue::glue_sql("
        SELECT DISTINCT user_id
        FROM user_presence
        WHERE record_id = {record_being_edited()$record_id}
          AND context = {id}
          AND field = {record_being_edited()$field}
          AND session_id != {session$token}
          AND last_seen > {ifelse(USE_SQLITE, \"datetime('now', '-10 seconds')\", \"NOW() - INTERVAL '15 seconds'\")};
      ", .con = get_db_pool())
      
      res <- get_db_query(query)
      if(is.data.frame(res) && nrow(res) > 0) return(res$user_id)
      return(character(0))
    })
    
    # 3. RENDER BANNER
    output$banner <- renderUI({
      if (is.null(record_being_edited()$record_id) || record_being_edited()$record_id == "") return(NULL)
      
      others <- active_others()
      if (length(others) > 0) {
        user_list <- paste(others, collapse = ", ")
        
        # Make the warning text context-aware
        warning_text <- if(is.null(record_being_edited()$field)) {
          paste(" Careful! Also viewing this record:", user_list)
        } else {
          paste(" Careful!", user_list, "is also editing the", record_being_edited()$field, "field.")
        }
        
        div(class = "alert alert-warning", 
            style = "padding: 10px; margin-bottom: 15px;",
            icon("users"), warning_text)
      } else {
        div(class = "alert alert-info", 
            style = "padding: 10px; margin-bottom: 15px; opacity: 0.8;",
            icon("user"), " You are the only one viewing this.")
      }
    })
    
    # 4. CLEANUP ON EXIT
    # We delete based on session_id AND context (which is `id`).
    session$onSessionEnded(function() {
      query <- glue::glue_sql("
        DELETE FROM user_presence 
        WHERE session_id = {session$token} AND context = {id};
      ", .con = get_db_pool())
      get_db_query(query)
    })
    
  })
}
