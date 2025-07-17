page_navbar(
  title = "CoC Project Rating and Ranking Tool",
  id = "nav",
  
  header = tagList(
    ## css, idle management, and dimension management --------
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    ## Enable shinyjs -----
    useShinyjs(),
    disconnectMessage(
      text = str_squish(
        "HORRT has crashed. Please submit an issue on GitHub and note the
          date and time (including timezone) in order to help the team diagnose the issue."
      ),
      overlayColour = '#F5F5F5',
      refresh = ""
    )
  ),
  # CoC Selection Page
  nav_panel(
    "Select CoC",
    card(
      card_header("Select your Continuum of Care"),
      selectInput("coc_select", "CoC Code",
                 choices = c("Please select" = "", sort(unique(hic_data$CoC_Code)))),
      actionButton("next_btn", "Next", class = "btn-primary")
    )
  ),
  
  # Review Projects
  nav_panel(
    "Review Projects",
    value = "review_projects",
    layout_sidebar(
      sidebar = sidebar(
        title = "Filters",
        width = 300,
        open = FALSE,
        selectInput("filter_funding_action", "Funding Action",
                   choices = c("All", "Renew", "New", "Expand", "Reallocate", "Ignore"),
                   multiple = TRUE),
        selectInput("filter_dv_renewal", "DV Renewal",
                   choices = c("All", "Yes", "No"),
                   multiple = TRUE),
        selectInput("filter_project_type", "Project Type",
                   choices = c("All", project_types),
                   multiple = TRUE),
        selectInput("filter_target_pop", "Target Population",
                   choices = c("All", target_populations),
                   multiple = TRUE),
        selectInput("filter_org", "Organization",
                   choices = c("All"),  # Will be updated in server
                   multiple = TRUE)
      ),
      card(
        card_header("Review Projects"),
        DTOutput("projects_table"),
        actionButton("add_project_btn", "Add New Project")
      )
    )
  ),
  
  # Customize Rating Criteria
  nav_panel(
    "Customize Rating Criteria",
    value = "rating_criteria",
    navset_card_tab(
      # Thresholds tab
      nav_panel(
        "Thresholds",
        card(
          checkboxGroupInput("coc_thresholds", "CoC Threshold Requirements",
                          choices = c(
                            "Housing First Approach",
                            "Participates in Coordinated Entry",
                            "Active Board Member",
                            "Submits APR timely",
                            "No unresolved monitoring findings"
                          ),
                          selected = c(
                            "Housing First Approach",
                            "Participates in Coordinated Entry",
                            "Active Board Member",
                            "Submits APR timely",
                            "No unresolved monitoring findings"
                          )),
          actionButton("add_threshold_btn", "Add New Threshold", class = "btn-primary")
        )
      ),
      
      # Renewal+Expansion Rating Factors tab
      nav_panel(
        "Renewal+Expansion Rating Factors",
        card(
          fluidRow(
            column(4,
                   selectInput("rating_project_type_filter", "Filter by Project Type",
                             choices = c("All", project_types),
                             multiple = TRUE,
                             selected = "All")
            ),
            column(4,
                   selectInput("rating_population_filter", "Filter by Special Population",
                             choices = c("All", target_populations),
                             multiple = TRUE,
                             selected = "All")
            )
          ),
          uiOutput("rating_factors_ui")
        )
      ),
      
      # New Project Rating Factors tab
      nav_panel(
        "New Project Rating Factors",
        card(
          selectInput("new_rating_population_filter", "Filter by Special Population",
                     choices = c("All", target_populations),
                     multiple = TRUE,
                     selected = "All"),
          uiOutput("new_project_rating_factors_ui")
        )
      )
    )
  ),

  # Individual Renewal/Expansion Rating
  nav_panel(
    "Rate Renewal Projects",
    value = "rate_renewal",
    div(
      style = "padding: 15px; background-color: #f8f9fa; border-radius: 5px; margin-bottom: 15px;",
      div(
        style = "display: flex; align-items: center; margin-bottom: 10px;",
        div(
          style = "width: 120px;",
          tags$label("Project Name:", class = "control-label")
        ),
        div(
          style = "flex-grow: 1;",
          selectInput("rate_project_select", NULL, choices = NULL)
        )
      ),
      uiOutput("project_info_sidebar")
    ),
    navset_card_tab(
      nav_panel(
        "Threshold Requirements",
        accordion(
          accordion_panel(
            "HUD Requirements",
            uiOutput("hud_requirements")
          ),
          accordion_panel(
            "CoC Requirements",
            uiOutput("coc_requirements")
          )
        ),
        card(
          card_footer(
            div(
              class = "d-grid gap-2",
              actionButton("save_threshold_ratings", "Save Threshold Ratings", 
                         class = "btn-primary")
            )
          )
        )
      ),
      nav_panel(
        "Rating Factors",
        uiOutput("project_rating_factors")
      ),
      nav_panel(
        "Summary",
        uiOutput("renewal_rating_summary")
      )
    )
  ),
  
  # Individual New Project Rating
  nav_panel(
    "Rate New Projects",
    value = "rate_new",
    div(
      style = "padding: 15px; background-color: #f8f9fa; border-radius: 5px; margin-bottom: 15px;",
      div(
        style = "display: flex; align-items: center; margin-bottom: 10px;",
        div(
          style = "width: 120px;",
          tags$label("Project Name:", class = "control-label")
        ),
        div(
          style = "flex-grow: 1;",
          selectInput("rate_new_project_select", NULL, choices = NULL)
        )
      ),
      uiOutput("new_project_info_sidebar")
    ),
    navset_card_tab(
      nav_panel(
        "Threshold Requirements",
        accordion(
          accordion_panel(
            "HUD Requirements",
            uiOutput("new_hud_requirements")
          ),
          accordion_panel(
            "CoC Requirements",
            uiOutput("new_coc_requirements")
          )
        ),
        card(
          card_footer(
            div(
              class = "d-grid gap-2",
              actionButton("save_new_threshold_ratings", "Save Threshold Ratings", 
                         class = "btn-primary")
            )
          )
        )
      ),
      nav_panel(
        "Rating Factors",
        uiOutput("new_project_rating_factors")
      ),
      nav_panel(
        "Summary",
        uiOutput("new_rating_summary")
      )
    )
  ),

  # Alternative Rating
  nav_panel(
    "Alternative Rating",
    value = "alternative_rating",
    card(
      DTOutput("alternative_rating_table")
    )
  ),

  # Funding Ceilings + Priorities
  nav_panel(
    "Funding Ceilings + Priorities",
    value = "funding_priorities",
    card(
	min_height=300,
      card_header("General Funding Information"),
      div(
        style = "padding: 15px;",
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          textInput("ard", "Annual Renewal Demand (ARD)", value = "0"),
          textInput("coc_bonus", "CoC Bonus", value = "0"),
          textInput("tier1", "Tier 1", value = "0"),
          textInput("adjusted_ard", "Adjusted ARD", value = "0"),
          textInput("yhdp_ard", "YHDP ARD", value = "0"),
          textInput("tier2", "Tier 2", value = "0"),
          textInput("dv_bonus", "DV Bonus", value = "0"),
          textInput("dv_ard", "DV ARD", value = "0")
        )
      )
    ),
    card(
	min_height=300,
      card_header("FY2024 HUD CoC Program NOFO Opportunities"),
      div(
        style = "padding: 15px;",
        layout_columns(
          col_widths = c(6, 6),
          div(
            h4("Project Types to Consider for CoC Bonus"),
            checkboxGroupInput(
              "coc_bonus_types",
              NULL,
              choices = c(
                "RRH for individuals" = "rrh_ind",
                "RRH for families" = "rrh_fam",
                "TH+RRH for individuals" = "th_rrh_ind",
                "TH+RRH for families" = "th_rrh_fam"
              )
            )
          ),
          div(
            h4("Project Types to Consider for DV Bonus"),
            checkboxGroupInput(
              "dv_bonus_types",
              NULL,
              choices = c(
                "RRH for individuals" = "rrh_ind",
                "RRH for families" = "rrh_fam",
                "TH+RRH for individuals" = "th_rrh_ind",
                "TH+RRH for families" = "th_rrh_fam"
              )
            )
          )
        )
      )
    ),
    div(
      style = "display: flex; gap: 20px;",
      div(
        style = "width: 250px;",
        card(
          card_header("Enable/Disable Populations"),
          checkboxGroupInput(
            "population_toggles",
            NULL,
            choices = c(
              "All Families" = "1",
              "DV Families" = "2",
              "Chronically Homeless Families" = "3",
              "Veteran Families" = "4",
              "Parenting Youth" = "5",
              "All Individuals" = "6",
              "DV Individuals" = "7",
              "Chronically Homeless Individuals" = "8",
              "Veteran Individuals" = "9",
              "Single Youth" = "10"
            ),
            selected = 1:10
          )
        )
      ),
      div(
        style = "flex-grow: 1;",
        card(
          card_header("Funding Ceilings and Priorities by Project Type and Population"),
          div(
            style = "padding: 15px;",
            DTOutput("priorities_table")
          )
        )
      )
    )
  )
)