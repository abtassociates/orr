initialize_inline_edit_table_ui <- function(
    data, 
    column_defs = list(), 
    tableID, 
    initial_filter, 
    formatting = list(), 
    colnames=NULL, 
    cols_to_disable = NULL,
    buttons = NULL,
    header_cb = NULL
) {
  # Determine factor/dropdown columns and user-editable columns
  factor_cols <- sapply(data, is.factor)
  
  # --- STEP 1: Prepare information for JavaScript ---
  # factor_info is a named list of each factor variable and its levels/choices
  # and will be converted to a JSON object and passed to the datatable callback.
  factor_names <- names(factor_cols)[factor_cols]
  factor_info <- lapply(data[, ..factor_names], levels)
  if(!is.null(colnames)) 
    names(factor_info) <- toupper(project_variable_labels[match(names(factor_info), names(project_variable_labels))])

  # column_defs adds classname for easier management
  column_defs[[length(column_defs) + 1]] <- list(
    targets = match(factor_names, names(data)) - 1,  # Vector of all indices
    className = 'factor-edit-cell'
  )
  
  column_defs[[length(column_defs) + 1]] <- list(
    targets = match(cols_to_disable, names(data)) - 1,  # Vector of all indices
    className = 'disabled dt-right'
  )
  
  # --- STEP 2: initComplete JS ---
  init_js <- sprintf("
    function(settings, json) {
      var table = this.api();
      var factorInfo = %s;
      var tableID = '%s';
      
      // Function to set cell value
      function setCellText(cell, val) {
        var $td = $(cell.node());
        $td.empty().text(val); // Remove dropdown, restore text
      }
      
      table.on('dblclick', 'td.factor-edit-cell', function(e) {
        var $td = $(this);
        if ($td.find('select').length) return; // already editing

        var cell = table.cell(this);
        var colIndex = cell.index().column;
        var colName = table.column(colIndex).header().innerText;
        
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
  
          // Add the dropdown to the cell
          $td.empty().append($select);
          $select.focus();
          
          // If user makes a change, we update the cell in several ways:
          // 1. set the text of the cell to whatever dropdown value they select
          // 2. set the cell data to the same value. This is so if they double-click the cell again, the new value will appear as the default selection
          // 3. trigger a cell_edit event on the server, which will:
              // 1. update the underlying datatable data (so that the new value will be preserved in sorting and filtering)
              // 2. update the database
          $select.on('change blur', function(e) {
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
    jsonlite::toJSON(factor_info), 
    tableID,
    header_cb
  )

  # --- STEP 3: datatable creation ---
  dt <- datatable(
    data,
    style = "default",
    extensions = c('Buttons', 'KeyTable'),
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
    filter = "top",
    escape = FALSE,
    selection = "none",
    rownames = FALSE,
    fillContainer = TRUE,
    options = list(
      dom = "Bt",
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
    ),
    callback = JS("
      $(document).on('mouseenter', '#projects_table table.dataTable tbody td', function() {
      $(this).css('cursor', 'pointer');
      $(this).attr('title', 'Double-click a cell to edit'); // Set tooltip
      });"
        )
    ) 
  
  # Add any passed in formatting
  for (f in formatting) {
    dt <- dt %>% f # needs to be %>% instead of |>
  }
  return(dt)
}