
mod_requests_ui <- function(id) {
  ns <- NS(id)
  #tagList(
    #nav_panel(
    #  "Requests",
      card(
        card_header(h4('Version Access Requests')),
        card_body(
          fillable = FALSE,
          helpText("Please select a row from the table below update a request."),
          DTOutput(ns('requests_dt'))|> shinycssloaders::withSpinner(),
          actionButton(ns('approve_request'), label='Approve', class='btn-success'),
          actionButton(ns('reject_request'), label='Reject', class = 'btn-danger'),
          uiOutput(ns('request_update_controls'))
        )
      )
    #)
  #)
}

mod_requests_server <- function(id, user_coc) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$requests_dt <- renderDT({
        req(user_coc$auth)
        cur_requests <- get_db_tbl('coc_version_requests') |>
          fsubset(coc_version_id %in% user_coc$coc_version_id) |># & request_status != "approved") |>
          join(get_db_tbl('coc_versions') |> fselect(coc_version_id, coc), how='left') |>
          fmutate(request_status = get_lookup_label(request_status, "request_status")) |>
          fselect(coc, coc_version_id, requesting_user, date_created, request_status)#, request_text, requesting_user, request_status)
        # actions <- purrr::map_chr(cur_requests$request_id, function(id_) {
        #   paste0(
        #     '<div class="btn-group" style="width: 75px;" role="group" aria-label="Basic example">
        #   <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" title="Edit" id = ', id_, ' style="margin: 0"><i class="fa fa-pencil-square-o"></i></button>
        #   <button class="btn btn-danger btn-sm delete_btn" data-toggle="tooltip" data-placement="top" title="Delete" id = ', id_, ' style="margin: 0"><i class="fa fa-trash-o"></i></button>
        # </div>'
        #   )
        # })
        
        datatable(cur_requests,
                  colnames = str_to_title(
                    str_replace_all(names(cur_requests),'_',' ')
                  ),
                  extensions = "Buttons", escape=-1,
                  options = list(dom = 'tip'), 
                  rownames = FALSE,
                  editable = FALSE)
      })
      
      observe({
        shinyjs::toggleState(id = 'approve_request', condition = length(input$requests_dt_rows_selected) > 0)
        shinyjs::toggleState(id = 'reject_request', condition = length(input$requests_dt_rows_selected) > 0)
      })
      
      output$request_update_controls <- renderUI({
        req(length(input$requests_dt_rows_selected) != 0)
        radioButtons('request_role', label='Role', choices = c('Viewer','Editor'))
      })
    }
  )
  
}
