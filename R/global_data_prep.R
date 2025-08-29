lookups <- get_db_tbl("lookups")
users <- get_db_tbl("users")
cocs <- get_db_tbl("cocs")
requests <- get_db_tbl('coc_instance_requests')
coc_instance_users <- get_db_query(
  "SELECT u.*, i.coc 
  FROM coc_instance_users u 
  LEFT JOIN coc_instances i 
  ON u.coc_instance_id = i.coc_instance_id"
)
hud_ard_report <- get_db_tbl("hud_ard_report")
main_project_types <- c("PSH", "RRH", "TH", "TH+RRH")

coc_nofo_opportunities <- get_db_tbl("coc_nofo_opportunities") %>%
  fmutate(
    funding_action = get_lookup_label(funding_action, "funding_action"),
    project_type = get_lookup_label(project_type, "project_type"),
    target_population = get_lookup_label(target_population, "target_population"),
    population_group = get_lookup_label(population_group, "population_group", "value_long"),
    bonus_type = get_lookup_label(bonus_type, "bonus_type"),
    
    full_text = str_trim(paste0(
      funding_action, " ",
      project_type, " ",
      fifelse(!is.na(target_population) | !is.na(population_group), "for ", ""), 
      fcoalesce(fifelse(target_population == "Chronically Homeless", "100% Dedicated PLUS or Chronically Homeless", paste0(target_population, " ")), ""),
      fcoalesce(population_group, "")
    ))
  )

# Cognito configuration
client_id <- Sys.getenv("cognito_client_id")
client_secret <- Sys.getenv("cognito_client_secret") 
base_cognito_url <- Sys.getenv("cognito_base_url")
region <- Sys.getenv("cognito_region")
redirect_uri <- rstudioapi::translateLocalUrl(
  url = "http://localhost:3000",
  absolute = TRUE
)

## generate redirect URL
aws_auth_redirect <-
  paste0(
    base_cognito_url,
    "oauth2/authorize?",
    "response_type=code&",
    "client_id=", client_id, "&",
    "redirect_uri=", redirect_uri, "&",
    "state=appredirect",
    "&scope=","openid+profile+aws.cognito.signin.user.admin"
  )

## generate logout URL
aws_auth_logout <-
  paste0(
    base_cognito_url, "logout?",
    "client_id=", client_id, "&",
    "logout_uri=", redirect_uri
  )

## generate sign up URL
aws_auth_signup <- paste0(
  base_cognito_url, "signup?",
  "response_type=code&",
  "client_id=", client_id, "&",
  "redirect_uri=", redirect_uri
)

## define user pool app client
app <- httr::oauth_app(appname = Sys.getenv("cognito_app_client_name"),
                 key = client_id,
                 secret = client_secret,
                 redirect_uri = redirect_uri)

## define cognito API oauth endpoint
cognito <- httr::oauth_endpoint(authorize = "authorize",
                          access = "token",
                          base_url = paste0(base_cognito_url, "oauth2"))


# function to get user attributes from Cognito such as first name, last name, CoC
get_user_attr <- function( access_token) {
  url <- paste0("https://cognito-idp.", region, ".amazonaws.com/")

  body <- list(
    AccessToken = access_token
  )
  
  # Build and perform request with httr2
  response <- request(url) |>
    req_headers(
      "X-Amz-Target" = "AWSCognitoIdentityProviderService.GetUser",
      "Content-Type" = "application/x-amz-json-1.1"
    ) |>
    req_body_raw(toJSON(body, auto_unbox = TRUE)) |>
    req_error(is_error = function(resp) FALSE) |>  # Handle errors manually
    req_perform()
  
  # Parse response (AWS returns custom content type, so we parse manually)
  response_text <- resp_body_string(response)
  parsed <- fromJSON(response_text)
  status_code <- resp_status(response)
  
  # Check if successful
  if (status_code == 200) {
    return(list(
      success = TRUE,
      message = paste0("✅ Success! User attributes have been returned."),
      user_attr = parsed$details$UserAttributes,
      details = parsed
    ))
  } else {
    # Error handling
    error_message <- if (!is.null(parsed$message)) {
      parsed$message
    } else if (!is.null(parsed$`__type`)) {
      parsed$`__type`
    } else {
      "Unknown error occurred"
    }
    
    return(list(
      success = FALSE,
      message = paste0("❌ Error: ", error_message),
      details = parsed
    ))
  }
}

retrieve_user_data <- function(user_code){
  
  failed_token <- FALSE
  
  ## exchange token from user logging in
  tryCatch({token_res <- oauth2.0_access_token(endpoint = cognito,
                                               app = app,
                                               code = user_code,
                                               #scope = 'openid+profile',
                                               user_params = list(client_id = client_id,
                                                                  grant_type = "authorization_code"),
                                               use_basic_auth = TRUE)},
           error = function(e){failed_token <<- TRUE})
  
 
  # check result status, make sure token is valid and that the process did not fail
  if (failed_token) {
    return(NULL)
  }
  
  # If the token did not fail, use the token to retrieve user information
  user_information <- get_user_attr(token_res$access_token)
  # user_information <- GET(url = paste0(base_cognito_url, "oauth2/userInfo"), 
  #                         add_headers(Authorization = paste("Bearer", token_res$access_token)))
  
  ## transpose so attributes are easier to pull
  user_attr <- user_information$details$UserAttributes %>% 
    tidyr::pivot_wider(names_from='Name',values_from='Value')
  
  return(user_attr)
}


# define a tibble of allwed users (this can also be read from a local file or from a database)
allowed_users <- data.frame(
  user_email = users$username)
