get_factor_info <- function(data, column_defs, colnames, cols_to_disable) {
  # Determine factor/dropdown columns and user-editable columns
  factor_cols <- sapply(data, is.factor)
  
  # --- STEP 1: Prepare information for JavaScript ---
  # factor_levels is a named list of each factor variable and its levels/choices
  # and will be converted to a JSON object and passed to the datatable callback.
  factor_names <- names(factor_cols)[factor_cols]
  factor_levels <- lapply(data[, ..factor_names], levels)
  if(!is.null(colnames)) 
    names(factor_levels) <- toupper(variable_labels[match(names(factor_levels), names(variable_labels))])
  
  # column_defs adds classname for easier management
  column_defs[[length(column_defs) + 1]] <- list(
    targets = match(factor_names, names(data)) - 1,  # Vector of all indices
    className = 'factor-edit-cell'
  )
  
  return(
    list(
      factor_levels = factor_levels,
      column_defs = column_defs
    )
  )
}

get_numeric_info <- function(data, column_defs) {
  
  # Add class for Numeric Validation
  # 2. Identify Numeric columns (excluding disabled ones)
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  
  integer_cols <- names(data)[sapply(data, is.integer)]
  
  column_defs[[length(column_defs) + 1]] <- list(
    targets = match(integer_cols, names(data)) - 1,
    className = 'numeric-edit-cell integer-edit-cell'
  )

  column_defs[[length(column_defs) + 1]] <- list(
    targets = match(setdiff(numeric_cols, integer_cols), names(data)) - 1,
    className = 'numeric-edit-cell'
  )
  
  return(column_defs)
}

get_init_js <- function(factor_levels, header_cb) {
  # 1. Read the JS file
  main_js <- get_js_script("init.js")
  
  # 2. Convert factor_levels to a JSON character string
  # auto_unbox ensures single elements aren't wrapped in JS arrays unnecessarily
  factor_json <- as.character(jsonlite::toJSON(factor_levels, auto_unbox = TRUE))
  
  # Handle NULL header_cb
  if (is.null(header_cb)) header_cb <- ""
  
  # 3. Replace the placeholders. 
  # CRITICAL: fixed = TRUE is required so JSON and JS chars aren't treated as Regex!
  main_js <- gsub("__FACTOR_INFO__", factor_json, main_js, fixed = TRUE)
  main_js <- gsub("__HEADER_CB__", header_cb, main_js, fixed = TRUE)
  
  return(main_js)
}

validate_numeric_entry <- function(df, col_name, val) {
  new_val <- ifelse(is.integer(df[[col_name]]), as.integer(val), as.numeric(val))
  
  max_val = fcase(
    grepl("BED", toupper(col_name)), 99999,
    toupper(col_name) == 'weighted_score', 100,
    grepl("FUNDING|AMOUNT", toupper(col_name)), 9999999999
  )
  
  if ((!is.na(new_val) && (new_val < 0 || new_val > max_val)) || (is.na(new_val) && !is.na(val))) {
    showNotification(glue::glue("Invalid input: Please enter a number between 0 and {prettyNum(max_val, big.mark=',')}", type = "error"))
    return(FALSE)
  }
  
  return(TRUE)
}

initialize_inline_edit_table_ui <- function(
    data, 
    column_defs = list(),
    initial_filter = NULL, 
    formatting = list(), 
    editable = list(
      target = "cell",
      disable = list(
        columns = match(
          cols_to_disable, 
          names(data)
        ) - 1
      )
    ),
    colnames=NULL, 
    cols_to_disable = NULL,
    buttons = NULL,
    header_cb = NULL,
    options = list(),
    filter = "top",
    escape = FALSE,
    selection = "none",
    rownames = FALSE,
    fillContainer = TRUE,
    callback_js = "return table;",
    extensions = c("Buttons","KeyTable"),
    ...
) {
  # --- STEP 1: handle factors as dropdowns ---
  # get the factor levels and add classes to column defs
  factor_info <- get_factor_info(data, column_defs, colnames, cols_to_disable)
  factor_levels <- factor_info$factor_levels
  column_defs <- factor_info$column_defs
  
  # assign classes to numeric cols
  column_defs <- get_numeric_info(data, column_defs)
  
  # handle cols to disable
  column_defs[[length(column_defs) + 1]] <- list(
    targets = match(cols_to_disable, names(data)) - 1,  # Vector of all indices
    className = 'disabled dt-right'
  )
  
  # use js to show the dropdowns
  init_js <- get_init_js(factor_info$factor_levels, header_cb)
  
  callback_js0 <- get_js_script("callback.js")
  callback_js <- if(callback_js != 'return table;') 
    paste0(callback_js0, callback_js)
  else
    callback_js0
  
  # --- STEP 1: handle user-specified options ---
  default_options <- list(
    dom = "tip",
    paging = FALSE,
    scrollY = "100%",  # Limit table height
    keys = TRUE,
    searchCols = initial_filter,
    columnDefs = column_defs,
    initComplete = htmlwidgets::JS(init_js),
    buttons = buttons
  )

  final_options <- modifyList(default_options, options)
  
  # --- STEP 2: datatable creation ---
  dt <- datatable(
    data,
    style = "default",
    extensions = extensions,
    colnames = colnames,
    editable = editable,
    options = final_options,
    filter = filter,
    escape = escape,
    selection = selection,
    rownames = rownames,
    fillContainer = fillContainer,
    callback = htmlwidgets::JS(callback_js),
    ...
  ) # end datatable 
  
  # Add any passed in formatting
  for (f in formatting) {
    dt <- dt %>% f # needs to be %>% instead of |>
  }
  
  return(dt)
}