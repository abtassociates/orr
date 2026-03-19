get_factor_info <- function(data, column_defs, colnames, cols_to_disable) {
  # Determine factor/dropdown columns and user-editable columns
  factor_cols <- sapply(data, is.factor)
  
  # --- STEP 1: Prepare information for JavaScript ---
  # factor_levels is a named list of each factor variable and its levels/choices
  # and will be converted to a JSON object and passed to the datatable callback.
  factor_names <- names(factor_cols)[factor_cols]
  factor_levels <- lapply(data[, ..factor_names], levels)
  if(!is.null(colnames)) 
    names(factor_levels) <- toupper(inventory_variable_labels[match(names(factor_levels), names(inventory_variable_labels))])

  # column_defs adds classname for easier management
  column_defs[[length(column_defs) + 1]] <- list(
    targets = match(factor_names, names(data)) - 1,  # Vector of all indices
    className = 'factor-edit-cell'
  )
  
  column_defs[[length(column_defs) + 1]] <- list(
    targets = match(cols_to_disable, names(data)) - 1,  # Vector of all indices
    className = 'disabled dt-right'
  )
  
  return(
    list(
      factor_levels = factor_levels,
      column_defs = column_defs
    )
  )
}

get_init_js <- function(factor_levels, tableID, has_double_header, header_cb) {
  sprintf("
    function(settings, json) {
      var table = this.api();
      var factorInfo = %s;
      var tableID = '%s';
      var has_double_header = '%s';
      
      // Build a map of column index -> compound column name from double header
      // Row 0 = top header (group), Row 1 = bottom header (sub-column)
      function getCompoundColName(colIndex) {
        var $thead = $(table.table().header());
        var headerRows = $thead.find('tr');
        
        if (headerRows.length < 2) {
          return $(headerRows[0]).find('th').eq(colIndex).text().trim();
        }
        
        // Count how many top-row cells have rowspan > 1 (they occupy a slot in
        // colIndex space but do NOT appear as a <th> in the second row)
        var topCells = $(headerRows[0]).find('th');
        var rowspanOffset = 0;
        var colCursor = 0;
        var groupName = '';
        
        topCells.each(function() {
          var span = parseInt($(this).attr('colspan') || 1);
          var rowspan = parseInt($(this).attr('rowspan') || 1);
          
          if (colIndex >= colCursor && colIndex < colCursor + span) {
            groupName = $(this).text().trim();
            return false; // break
          }
          
          if (rowspan > 1) rowspanOffset += span; // these won't appear in row 2
          colCursor += span;
        });
        
        // The bottom row's <th> list is shorter by rowspanOffset columns
        var bottomRowIndex = colIndex - rowspanOffset;
        var subName = $(headerRows[1]).find('th').eq(bottomRowIndex).text().trim();
        
        return groupName && groupName !== subName
          ? groupName.toUpperCase() + '_' + subName.toLowerCase()
          : subName.toLowerCase();
      }
      
      // Function to set cell value
      function setCellText(cell, val) {
        var $td = $(cell.node());
        $td.empty().text(val); // Remove dropdown, restore text
      }
      
      table.on('dblclick', 'td.factor-edit-cell', function(e) {
        e.stopImmediatePropagation();
      
        var $td = $(this);
        if ($td.find('select').length) return; // already editing
debugger;
        var cell = table.cell(this);
        var colIndex = cell.index().column;
        var colName = has_double_header == 'TRUE' ? getCompoundColName(colIndex) : table.column(colIndex).header().innerText.toUpperCase();
        // var colName = table.column(colIndex).header().innerText;
        
        if (factorInfo[colName]) {
          var choices = factorInfo[colName];
          var currentVal = cell.data();

          // Build the dropdown
          var $select = $('<select></select>').css('width', '100%%');
          $.each(choices, function(i, value) {
            var $opt = $('<option></option>').val(value).text(value);
            if (value == currentVal) $opt.prop('selected', true);
            $select.append($opt);
          });
  debugger;
          // Add the dropdown to the cell
          $td.empty().append($select);
          $select.focus();
          
          // If user makes a change, we update the cell in several ways:
          // 1. set the text of the cell to whatever dropdown value they select
          // 2. set the cell data to the same value. This is so if they double-click the cell again, the new value will appear as the default selection
          // 3. trigger a cell_edit event on the server, which will:
              // 1. update the underlying datatable data (so that the new value will be preserved in sorting and filtering)
              // 2. update the database
          $select.off('change blur').on('change blur', function(e) {
            setCellText(cell, this.value); // Update cell text
            cell.data(this.value); // Update cell data
            Shiny.setInputValue(tableID + '_cell_edit', { // Trigger cell_edit server event
              row: cell.index().row + 1,
              col: cell.index().column,
              value: this.value,
              oldValue: currentVal,
              project_id: table.cells(cell.index().row, 0).data()[0],
            }, {priority: 'event'});
          });
          
          $('input').on('keydown', function(e) {
          //$select.off('keydown').on('keydown', function(e) {
          debugger;
            if (e.key === 'Escape') {
              setCellText(cell, currentVal); // On escape, revert to old value
            } else if (e.key === 'Enter') {
              $(this).blur(); // Trigger the change/blur event
            }
          });
        }
      }); /*end double-click*/
      
      %s
    }", 
    jsonlite::toJSON(factor_levels), 
    tableID,
    has_double_header,
    header_cb %||% ""
  )
}
initialize_inline_edit_table_ui <- function(
    data, 
    column_defs = list(), 
    tableID, 
    initial_filter = NULL, 
    formatting = list(), 
    colnames=NULL, 
    cols_to_disable = NULL,
    buttons = NULL,
    header_cb = NULL,
    options = list(),
    extensions = list(),
    filter = "top",
    escape = FALSE,
    selection = "none",
    rownames = FALSE,
    fillContainer = TRUE,
    callback_js = NULL,
    has_double_header = FALSE,
    ...
) {
  # --- STEP 1: handle factors as dropdowns ---
  # get the factor levels and add classes to column defs
  factor_info <- get_factor_info(data, column_defs, colnames, cols_to_disable)
  factor_levels <- factor_info$factor_levels
  column_defs <- factor_info$column_defs
  
  # use js to show the dropdowns
  init_js <- get_init_js(factor_info$factor_levels, tableID, has_double_header, header_cb)
  
  # --- STEP 1: handle user-specified options ---
  default_options <- list(
    dom = "tip",
    paging = FALSE,
    scrollY = "100%",  # Limit table height
    keys = TRUE,
    searchCols = initial_filter,
    columnDefs = column_defs,
    initComplete = DT::JS(init_js),
    buttons = buttons
    # rowCallback = JS(c(
    #   "function(row, data){",
    #   "  for(var i=0; i<data.length; i++){",
    #   "    if(data[i] === null){",
    #   "      $('td:eq('+i+')', row).html('NA')",
    #   "        .css({'color': 'rgb(151,151,151)', 'font-style': 'italic'});",
    #   "    }",
    #   "  }",
    #   "}"  
    # ))
  )
  final_options <- modifyList(default_options, options)
  
  # --- STEP 3: datatable creation ---
  dt <- datatable(
    data,
    style = "default",
    extensions = extensions,
    colnames = colnames,
    editable = list(
      target = "cell",
      disable = list(
        columns = match(
          cols_to_disable, 
          names(data)
        ) - 1
      )
    ),
    options = final_options,
    filter = filter,
    escape = escape,
    selection = selection,
    rownames = rownames,
    fillContainer = fillContainer,
    callback = JS(callback_js),
    ...
  ) # end datatable 
  
  # Add any passed in formatting
  for (f in formatting) {
    dt <- dt %>% f # needs to be %>% instead of |>
  }
  return(dt)
}