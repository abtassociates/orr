
mod_requests_ui <- function(id) {
  ns <- NS(id)
  #tagList(
    #nav_panel(
    #  "Requests",
      card(
        card_header(h4('Version Access Requests')),
        card_body(
          fillable = FALSE,
          helpText("Please select a row from the table below to update a request."),
          br(),
          br(),
          actionGroupButtons(
            c(ns("filter_sent"), ns("filter_approved"), ns("filter_rejected")),
            c("Outstanding", "Approved", "Rejected"),
            status = "default"
          ),
          DTOutput(ns('requests_dt'))|> shinycssloaders::withSpinner(),
          actionButton(ns('approve_request'), label='Approve', class='btn-success'),
          actionButton(ns('reject_request'), label='Reject', class = 'btn-danger')
        )
      )
    #)
  #)
}

mod_requests_server <- function(id, user_coc) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    cur_requests <- reactiveVal(NULL)
    
    all_requests <- reactive({
      req(user_coc$auth)
      
      user_versions <- coc_version_users |>
        fsubset(username == user_coc$username)
      
      get_db_tbl('coc_version_requests') |>
        fsubset(coc_version_id %in% user_versions$coc_version_id) |># & request_status != "approved") |>
        join(get_db_tbl('coc_versions') |> fselect(coc_version_id, coc), how='left') |>
        fmutate(request_status = get_lookup_label(request_status, "request_status")) |>
        fselect(coc_request_id, coc, coc_version_id, created_by, date_created, request_status) |>
        roworder(-request_status)
    })
    
    observe({
      req(user_coc$auth)
      cur_requests(all_requests())
    })
    
    cur_requests_proxy <- dataTableProxy(ns("requests_dt"))
    observe({
      replaceData(cur_requests_proxy, cur_requests())
    })
    
    output$requests_dt <- renderDT({
      req(user_coc$auth)
      
      datatable(
        isolate(cur_requests()),
        colnames = str_to_title(
          str_replace_all(names(cur_requests()),'_',' ')
        ),
        escape=-1,
        style = 'default',
        options = list(
          dom = 'Bfrtip',
          columnDefs = list(
            list(targets=0, className = "hidden")
          ),
          language = list(
            zeroRecords = "No outstanding requests"
          )
        ), 
        rownames = FALSE,
        editable = FALSE
      )
    })
    
    # Toggle Approve/Reject buttons depending on whether user has selected a row or not
    observe({
      req(user_coc$auth)
      req(nrow(cur_requests()) > 0)

      has_outstanding_requests <- any(
        cur_requests()[input$requests_dt_rows_selected]$request_status == "Sent"
      )
      shinyjs::toggle(id = 'approve_request', condition = length(input$requests_dt_rows_selected) > 0 && has_outstanding_requests)
      shinyjs::toggle(id = 'reject_request', condition = length(input$requests_dt_rows_selected) > 0 && has_outstanding_requests)
    })
    
    # Updating DB
    update_request <- function(status) {
      request_status_num <- get_lookup_refid(status, "request_status")
      selected_requests <- cur_requests()[input$requests_dt_rows_selected]
      
      apply(selected_requests, 1, function(row) {
        # Set Status in Requests table
        DBI::dbExecute(
          DB_CON,
          "UPDATE coc_version_requests 
          SET request_status = $1, date_updated = CURRENT_TIMESTAMP, updated_by = $2
          WHERE coc_request_id = $3", 
          params = list(request_status_num, user_coc$username, row[["coc_request_id"]])
        )
        
        # Create version user
        user_role_num <- get_lookup_refid("Editor", "coc_version_role")
        DBI::dbAppendTable(
          DB_CON,
          "coc_version_users",
          data.table(
            coc_version_id = row[["coc_version_id"]],
            username = row[["created_by"]],
            coc_version_role = user_role_num,  # Owner, Owner, Editor, Owner
            created_by = user_coc$username,
            date_created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            date_updated = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            updated_by = user_coc$username
          )
        )
      })
        
      # Updaet datatable proxy
      cur_requests(
        cur_requests() |> 
          fmutate(
            request_status = fifelse(
              coc_request_id %in% selected_requests$coc_request_id, 
              status, 
              request_status
            )
          )
      )
    }
    observeEvent(input$approve_request, {update_request("Approved")})
    observeEvent(input$reject_request, {update_request("Rejected")})
    
    filter_requests <- function(status) {
      cur_requests(
        all_requests() |> 
          fsubset(request_status == status)
      )
    }
    observeEvent(input$filter_sent, {filter_requests("Sent")})
    observeEvent(input$filter_approved, {filter_requests("Approved")})
    observeEvent(input$filter_rejected, {filter_requests("Rejected")})
  }
)}
