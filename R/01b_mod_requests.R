
mod_requests_ui <- function(id) {
  ns <- NS(id)
  #tagList(
    #nav_panel(
    #  "Requests",
      card(
        #card_header(h4('Version Access Requests')),
        card_header(h4("Version Access Requests", 
                       div(style = "float: right;",
                           actionBttn(ns('refresh_requests_tbl'), label="Refresh", color="primary", size="xs", icon=icon('refresh')))
        )),
        card_body(
          fillable = FALSE,
          helpText("Please select a row from the table below to update a request."),
          br(),
          br(),
          radioGroupButtons(
            ns('request_filters'),
            choices = c('Received','Sent','Approved','Rejected'),
            selected = NULL,
            status = "primary"
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
      
      user_versions <-  get_db_query(
        "SELECT v.*, u.username, u.coc_version_role
          FROM coc_versions v
          LEFT JOIN coc_version_users u
          ON v.coc_version_id = u.coc_version_id"
      ) |> #COC_VERSION_USERS |>
        fsubset(username == user_coc$username)
      
      get_db_tbl('coc_version_requests') |>
        fsubset(created_by == user_coc$username | coc_version_id %in% user_versions$coc_version_id) |>
        join(get_db_tbl('coc_versions') |> fselect(coc_version_id, coc, coc_version_name), how='left') |>
        fmutate(request_status = get_lookup_label(request_status, "request_status")) |>
        join(get_db_tbl('cocs'), how = 'left', on = c('coc' = 'coc_code')) |>
        fselect(coc_request_id, coc_version_name, coc_name, coc, coc_version_id, created_by, date_created, request_status) |>
        roworder(-request_status)
    })
    
    observe({
      req(user_coc$auth)
      cur_requests(all_requests())
    })
    
    observeEvent(input$refresh_requests_tbl, {
      showNotification("Refreshing Requests table!", type = "message")
      cur_requests(all_requests())
    })
    
    cur_requests_proxy <- dataTableProxy(ns("requests_dt"))
    observe({
      replaceData(cur_requests_proxy, cur_requests())
    })
    
    output$requests_dt <- renderDT({
      req(user_coc$auth)
      
      # create "sent to " field when category is Sent
      cols_to_hide <- which(names(cur_requests()) %in% c('coc_request_id', 'coc_version_id')) - 1
      
      datatable(
        isolate(cur_requests()),
        colnames = unname(requests_variable_labels[names(cur_requests())]),
        escape=-1,
        style = 'default',
        options = list(
          dom = 'tip',
          autoWidth = FALSE,
          columnDefs = list(
            list(targets= cols_to_hide, className = "hidden", visible = FALSE)
          ),
          language = list(
            zeroRecords = "No outstanding requests"
          )
        ), 
        rownames = FALSE,
        editable = FALSE
      ) %>% 
        formatDate(
          columns = c('date_created'),
          method = 'toLocaleString'
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
      
      update_params <- lapply(seq_row(selected_requests), function(i) {
        list(
          request_status_num, 
          user_coc$username, 
          selected_requests[i]$coc_request_id,
          selected_requests[i]$date_updated
        )
      })
      
      dbWithTransaction(DB_POOL, {
        DBI::dbExecute(
          DB_POOL, 
          glue::glue(
            "UPDATE coc_version_requests 
              SET request_status = $1, date_updated = CURRENT_TIMESTAMP, updated_by = $2 
                {ifelse(status == 'Approved', '', ', reason_for_rejection = $3')}
              WHERE coc_request_id = $3 AND date_updated = $4"
          ), 
          params = update_params
        )
        
        user_role_num <- get_lookup_refid("Editor", "coc_version_role")
        current_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        
        if(status == 'Approved') {
          new_users <- data.table(
            coc_version_id = approved_requests$coc_version_id,
            username = approved_requests$created_by,
            coc_version_role = user_role_num,
            created_by = user_coc$username,
            date_created = current_time,
            date_updated = current_time,
            updated_by = user_coc$username
          )
          
          DBI::dbAppendTable(DB_POOL, "coc_version_users", new_users)
        }
      })
        
      # Update datatable proxy
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
    observeEvent(input$approve_request, {
      showModal(
        modalDialog(
          title = 'Confirm Approval',
          "Please confirm that you would like to approve access to the selected CoC versions.",
          footer = tagList(
            actionButton(ns('confirm_approve'), 'Confirm'),
            modalButton('Cancel')
          )
        )
      )
    })
    
    observeEvent(input$confirm_approve, {
      update_request("Approved")
      removeModal()
      showNotification('Request approved.', type='message') 
    })
    
    observeEvent(input$reject_request, {
      showModal(
        modalDialog(
          title = 'Confirm Rejection',
          radioButtons(ns('rej_reason'), label = 'Please specify a reason for rejection and confirm.',
                      choices = get_db_tbl('request_rejection_reasons')$request_rejection_reason),
          # conditionalPanel(
          #   condition = 'input.rej_reason == "Other"',
          #   textInput('rej_other_specify', "Other - please specify:"),
          # ),
          footer = tagList(
            actionButton(ns('confirm_reject'), 'Confirm'),
            modalButton('Cancel')
          )
        )
      )
      
    })
    
    observeEvent(input$confirm_reject, {
      update_request("Rejected")
      removeModal()
      showNotification('Request rejected.', type = 'warning')
    })
    
    filter_requests <- function(status) {
      cur_requests(
        if(status == 'Received'){
          all_requests() |> 
            fsubset(request_status == 'Sent' & created_by != user_coc$username)
        } else if(status == 'Sent') {
          all_requests() |> 
            fsubset(created_by == user_coc$username)
        } else if(status == 'Approved'){
          all_requests() |> 
            fsubset(request_status == 'Approved' & created_by != user_coc$username)
        } else if(status == 'Rejected') {
          all_requests() |> 
            fsubset(request_status == 'Rejected' & created_by != user_coc$username)
        }
       
      )
    }
    
    observeEvent(input$request_filters, {
      filter_requests(status = input$request_filters)
    })
  
  }
)}
