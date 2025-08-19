initialize_table_ui <- function(data, user_columns, tableID, initial_filter) {
  # Determine factor/dropdown columns and user-editable columns
  factor_cols <- sapply(data, is.factor)
  
  # --- STEP 1: Prepare information for JavaScript ---
  # Create a list that maps 0-based column indices to their choices.
  # This will be converted to a JSON object and passed to the datatable callback.
  factor_info <- list()
  
  column_defs <- list(list(
    targets = which(names(data) %in% user_columns),
    className = 'green-background'
  ))
  
  for (col_name in names(factor_cols)[factor_cols]) {
    col_index <- which(names(data) == col_name) - 1
    choices <- levels(data[[col_name]])
    factor_info[[as.character(col_index)]] <- choices

    column_defs[[length(column_defs) + 1]] <- list(
      targets = col_index,
      className = 'factor-edit-cell',
      render = DT::JS(sprintf("
        function(data, type, row, meta) {
          if (type === 'display') {
            var map = %s;
            return map[data] || data;
          }
          return data;
        }",
        jsonlite::toJSON(choices)
      ))
    )
  }
  
  # --- STEP 2: initComplete JS ---
  init_js <- sprintf("
    function(settings, json) {
      var table = this.api();
      var factorInfo = %s;
      var tableID = '%s';
      
      // Function to revert cell to its original state
      function revertCell(cell, originalData) {
        var $td = $(cell.node());
        $td.empty().text(originalData); // Remove dropdown, restore text
      }
      
      table.on('dblclick', 'td.factor-edit-cell', function(e) {
        var cell = table.cell(this);
        var colIndex = cell.index().column;
        var $td = $(this);
  
        if ($td.find('select').length) return; // already editing

        if (factorInfo[colIndex]) {
          var choices = factorInfo[colIndex];
          var currentVal = cell.data();
debugger;
          // Build the dropdown
          var $select = $('<select></select>').css('width', '100%%');
          $.each(choices, function(i, value) {
            var $opt = $('<option></option>').val(value).text(value);
            if (value == currentVal) $opt.prop('selected', true);
            $select.append($opt);
          });
  
          // Add the dropdown to the cell
          $td.empty().append($select);
          $select.focus();
          
          // If user makes a change, trigger a cell_edit event
          $select.on('change blur', function(e) {
            revertCell(cell, currentVal);
            Shiny.setInputValue(tableID + '_cell_edit', {
              row: cell.index().row + 1,
              col: cell.index().column,
              value: this.value,
              oldValue: currentVal,
              project_id: table.cells(cell.index().row, 0).data()[0],
            }, {priority: 'event'});
          });
          
          $('input').on('keydown', function(e) {
            if (e.key === 'Enter' && !e.altKey && !e.shiftKey && !e.ctrlKey) {
              e.preventDefault(); // stop form submission if needed
              $(this).blur();     // trigger blur
            }
          });
        }
      });
    }", 
    jsonlite::toJSON(factor_info), 
    tableID
  )
  
  # --- STEP 3: datatable creation ---
  dt <- datatable(
    data,
    editable = "cell",
    filter = "top",
    escape = FALSE,
    selection = "none",
    rownames = FALSE,
    fillContainer = TRUE,
    options = list(
      scrollY = "100%",  # Limit table height
      searchCols = initial_filter,
      columnDefs = column_defs,
      initComplete = DT::JS(init_js)
    )
  ) %>%
    formatStyle(
      columns = c(2,3), 
      `white-space` = "nowrap",
      `overflow` = "hidden",
      `max-width` = "400px"
    )
  
  return(dt)
}