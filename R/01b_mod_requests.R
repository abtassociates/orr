
mod_requests_ui <- function(id) {
  ns <- NS(id)
  #tagList(
    #nav_panel(
    #  "Requests",
      card(
        #card_header(h4('Version Access Requests')),
        card_header(h4("Version Access Requests"
                       # div(style = "float: right;",
                           # actionBttn(ns('refresh_requests_tbl'), label="Refresh", color="primary", size="xs", icon=icon('refresh')))
        )),
        card_body(
          fillable = FALSE,
          helpText("Please select a row from the table below to update a request."),
          br(),
          br(),
          radioGroupButtons(
            ns('request_filters'),
            choices = c('Received','Sent','Approved','Rejected'),
            selected = "Received",
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

mod_requests_server <- function(id, user_coc, module_returns) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    cur_requests <- reactiveVal(NULL)

    observeEvent(c(user_coc$auth, module_returns$updated_request), {
      cur_requests(
        get_all_requests(user_coc$username) %>%
          fmutate(
            request_status = get_lookup_label(request_status, ref_type = "request_status")
          )
      )
    })
    
    filter_requests <- function(dt, status) {
      if(status == 'Received'){
        dt |> 
          fsubset(request_status == 'Sent' & created_by != user_coc$username)
      } else if(status == 'Sent') {
        dt |> 
          fsubset(created_by == user_coc$username)
      } else {
        dt |> 
          fsubset(request_status == status & created_by != user_coc$username)
      }
    }
    
    cur_requests_filtered <- reactive({
      if(is.null(input$request_filters)) cur_requests()
      else filter_requests(cur_requests(), status = input$request_filters)
    })
    
    cur_requests_proxy <- dataTableProxy("requests_dt", session=session)
    observe({
      req(cur_requests_filtered())
      replaceData(cur_requests_proxy, cur_requests_filtered(), resetPaging = FALSE, rownames = FALSE)
    })
    
    output$requests_dt <- renderDT({
      req(user_coc$auth)
      
      # create "sent to " field when category is Sent
      cols_to_hide <- which(names(cur_requests()) %in% c('coc_request_id', 'coc_version_id')) - 1
      
      datatable(
        isolate(cur_requests_filtered()),
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
          columns = c('date_created', 'date_updated'),
          method = 'toLocaleString'
        )
    })
    
    # Toggle Approve/Reject buttons depending on whether user has selected a row or not
    observe({
      req(user_coc$auth)
      
      has_outstanding_requests <- any(
        cur_requests_filtered()[input$requests_dt_rows_selected]$request_status == "Sent"
      )
      
      condition <- length(input$requests_dt_rows_selected) > 0 && 
        has_outstanding_requests &&
        input$request_filters == "Received"
      
      shinyjs::toggle(id = 'approve_request', condition = condition)
      shinyjs::toggle(id = 'reject_request', condition = condition)
    })
    
    # Updating DB
    update_request <- function(status) {
      request_status_num <- get_lookup_refid(status, "request_status")
      selected_requests <- cur_requests_filtered()[input$requests_dt_rows_selected]

      update_params <- selected_requests |>
        fmutate(
          "request_status_num" = request_status_num,
          "username" = user_coc$username,
          "reason_for_rejection" = input$rej_reason
        ) |>
        fselect(
          request_status_num,
          username,
          coc_request_id,
          date_updated
        )
      
      pool::poolWithTransaction(get_db_pool(), function(p) {
        update_request_status(p, update_params)
        
        if(status == 'Approved') {
          new_version_users <- selected_requests |>
            fmutate(
              username = selected_requests$created_by,
              coc_version_role = get_lookup_refid("Editor", "coc_version_role")
            ) |>
            add_user_stamp(user_coc, is_new = TRUE) |>
            fselect(coc_version_id, username, coc_version_role, created_by, updated_by)
          
          DBI::dbAppendTable(p,"coc_version_users",new_version_users)
        }
        
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
      })
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
                      choices = LOOKUPS[reference_type == "request_rejection_reason"]$value),
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
  }
)}
