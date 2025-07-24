
mod_requests_ui <- function(id) {
  ns <- NS(id)
  #tagList(
    nav_panel(
      "Requests",
      card(
        card_body(
          fillable = FALSE,
          DTOutput(ns('requests_dt')),
          actionButton(ns('approve_request')),
          actionButton(ns('reject_request')),
          uiOutput(ns('request_update_controls'))
        )
      )
    )
  #)
}

mod_requests_server <- function(id, coc_iu) {
  moduleServer(
    id,
    function(input, output, session) {
      output$requests_dt <- renderDT({
        cur_requests <- requests |>
          fsubset(coc_instance_id %in% coc_iu()$coc_instance_id) |># & request_status != "approved") |>
          fselect(request_id, coc, request_text,requesting_user, request_status)
        actions <- purrr::map_chr(cur_requests$request_id, function(id_) {
          paste0(
            '<div class="btn-group" style="width: 75px;" role="group" aria-label="Basic example">
          <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" title="Edit" id = ', id_, ' style="margin: 0"><i class="fa fa-pencil-square-o"></i></button>
          <button class="btn btn-danger btn-sm delete_btn" data-toggle="tooltip" data-placement="top" title="Delete" id = ', id_, ' style="margin: 0"><i class="fa fa-trash-o"></i></button>
        </div>'
          )
        })
        
        datatable(cbind(actions,cur_requests), extensions = "Buttons", escape=-1)
      })
      
      observe({
        if(length(input$requests_dt_rows_selected)==0){
          shinyjs::disable(id = 'update_request')
        } else {
          shinyjs::enable(id = 'update_request')
        }
      })
      
      output$request_update_controls <- renderUI({
        req(length(input$requests_dt_rows_selected)==0)
        radioButtons('request_role', label='Role', choices = c('Viewer','Editor'))
      })
    }
  )
  
}
