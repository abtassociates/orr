page_navbar(
  title = "CoC Project Rating and Ranking Tool",
  id = "nav",
  
  header = tagList(
    ## css, idle management, and dimension management --------
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    ## Enable shinyjs -----
    shinyjs::useShinyjs(),
    disconnectMessage(
      text = str_squish(
        "HORRT has crashed. Please submit an issue on GitHub and note the
          date and time (including timezone) in order to help the team diagnose the issue."
      ),
      overlayColour = '#F5F5F5',
      refresh = ""
    )
  ),
  
  mod_coc_selection_ui("coc_selection"),
  mod_inventory_ui("inventory"),
  mod_rating_criteria_ui("rating_criteria"),
  mod_renewal_rating_ui("renewal_rating"),
  mod_new_rating_ui("new_rating"),
  mod_alternative_rating_ui("bulk_rating"),
  mod_funding_priorities_ui("funding_priorities"),
  mod_final_review_ui("final_review"),
  mod_ranking_ui("ranking")
)