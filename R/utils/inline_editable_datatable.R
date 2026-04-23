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

get_init_js <- function(factor_levels, tableID, has_double_header, header_cb) {
  main_js <- sprintf(
    "function(settings, json) {
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
        let formatted_val = getFormattedVal(getColName(cell), val);
        $td.empty().text(formatted_val); // Remove dropdown, restore text
      }
      
      function getColName(cell) {
        var colIndex = cell.index().column;
        let colName = table.column(colIndex).header().innerText.toUpperCase();
        if (has_double_header == 'TRUE') {
            colName = getCompoundColName(colIndex);
        } else if (tableID == 'rating-alternative-alternative_rating_table') {
            colName = table.column(colIndex).header().childNodes[0].wholeText;
        }
        
        colName = colName.replace(' Ⓘ','');
        return(colName);
      }
      
      function noDataChange(val, cell) {
        return(getRawVal(val) == getRawVal(cell.data()));
      }
      function getRawVal(val) {
        if(val === null) return(val);
        let rv = parseFloat(val.toString().replace(/[^\\d.]/g, ''));
        return(rv);
      }
      
      function getFormattedVal(colName, val) {
        let fv = is_funding_col(colName) ? formatUSD(val) : val.toLocaleString('en-US');
        return(fv);
      }
      
      // used to trim values that are longer than they shoul dbe
      function get_max_length(colName) {
        let maxLength;
        if(colName.toUpperCase().includes('BED')) maxLength = 5;
        else if(colName.toUpperCase() == 'WEIGHTED_SCORE') maxLength = 3;
        else if(is_funding_col(colName)) maxLength = 9;
        return(maxLength);
      }
      
      // trim values
      function trim_val(colName, val, max_length = null) {
        let c = colName;
        
        let maxLength;
        if(max_length) maxLength = max_length;
        else maxLength = get_max_length(c);
        
        if (val.length > maxLength) val = val.slice(0, maxLength);
        return(val);
      }
  
      function formatUSD(amount) {
        if(amount === null) return;
        
        amount = Number(amount);
        
        if (typeof amount !== 'number' || isNaN(amount)) {
            throw new Error('Invalid input: amount must be a valid number.');
        }
    
        return new Intl.NumberFormat('en-US', {
            style: 'currency',
            currency: 'USD',
            minimumFractionDigits: 0,
            maximumFractionDigits: 0
        }).format(amount);
      }
      
      function is_funding_col(colName) {
        let c = colName.toUpperCase();
        if(c == 'FUNDING ACTION') return(false);
        return(c.includes('FUNDING') || c.includes('AMOUNT'));
      }
      
      function revertCell(cell) {
        oldVal = cell.data();
        setCellText(cell, oldVal);
      }
      
      // If user makes a change, we update in several ways:
      // 1. set the text of the cell to whatever dropdown value they select
      // 2. trigger a cell_edit event on the server, which will:
          // 1. update the underlying datatable data (so that the new value will be preserved in sorting and filtering)
          // 2. update the database
      // 3. set the cell data to the same value. This is so if they double-click the cell again, the new value will appear as the default selection
      function updateTableAndShiny(cell, val) {
        var colName = getColName(cell).toUpperCase();
        setCellText(cell, val);
        alertShiny(cell, val);
        cell.data(val);
      }
      
      function alertShiny(cell, val) {
        Shiny.setInputValue(tableID + '_cell_edit', {
            row: cell.index().row + 1,
            col: cell.index().column,
            value: val,
            oldValue: cell.data(),
            project_id: table.cells(cell.index().row, 0).data()[0],
          }, {priority: 'event'});
      }
      
      function isNumeric(str) {
        str = str.replace(/[^\\d.]/g, '');
        if (typeof str !== 'string' || str.trim() === '') return false; // reject empty/whitespace
        const num = Number(str);
        return !Number.isNaN(num);
      }
      
      // Handle keyboard pasting directly to cell
      document.addEventListener('paste', function(e) {
        // 1. Check if your specific cell is currently 'focused' by Shiny
        var $td = $('td.focus');
        
        // If no such cell is focused, ignore the paste and let it act normally
        if ($td.length === 0 || $td.find('input').length) return;
        
        e.preventDefault();
        
        var cell = table.cell($td);
        
        // 3. Get the clipboard data. 
        var clipboardData = e.clipboardData || window.clipboardData;
        var pastedValue = clipboardData.getData('text/plain');
        
        // If they tried pasting a number into a non-numeric cell, revert
        if(isNumeric(pastedValue) && !$td.hasClass('numeric-edit-cell')) return;
        
        // If they tried pasting an invlaid value into a factor cell, revert
        if($td.hasClass('factor-edit-cell')) {
          var colName = getColName(cell);
          if (!factorInfo[colName].includes(pastedValue)) return;
        }
        
        // Don't process unchanged data
        if(noDataChange(pastedValue, cell)) return;
        
        if($td.hasClass('numeric-edit-cell')) pastedValue = getRawVal(pastedValue);
        
        updateTableAndShiny(cell, pastedValue);
      }, true);
      
      table.on('dblclick', 'td', function(e) {
        var cell = table.cell(this);
        Shiny.setInputValue(tableID + '_cell_being_edited', {
          row: cell.index().row + 1,
          col: cell.index().column
        }, {priority: 'event'});
      });
    ",
    jsonlite::toJSON(factor_levels), 
    tableID,
    has_double_header
  )
  
  numeric_js <-  "
    table.on('dblclick', 'td.numeric-edit-cell', function(e) {
      //e.stopImmediatePropagation();
      var $td = $(this);
      setTimeout(function() {
        //if ($td.find('input').length) return; // already editing
    
        var cell = table.cell($td);
        var colName = getColName(cell).toUpperCase();
  
        // --- 1. Get the input ---
        var isInteger = $td.hasClass('integer-edit-cell');
        var $input = $td.find('input[type=number]')
          .attr({'min': '0', 'max' : '9'.repeat(get_max_length(colName)), 'step': '1'});
    
        // --- 2. Enforce the Character Limit! ---
        $input.off('input').on('input', function() {
          var val = this.value;
          
          if(is_funding_col(colName)) {
            if (val.includes('.')) {
              var parts = val.split('.');
              parts[0] = trim_val(colName, parts[0]);
              parts[1] = trim_val(colName, parts[1], 2);
              this.value = parts[0] + '.' + parts[1]; // stitch back together
            } else {
              this.value = trim_val(colName, val);
            }
          } else {
            this.value = trim_val(colName, val);
          }
        });
        
        $input.off('keydown').on('keydown', function(e) {
          // --- Validation ---
          // Prevent '-' (minus sign) and 'e' (scientific notation)
          if (e.key === '-' || e.key === 'e' || e.key === 'E')
            e.preventDefault();
              
          // IF it's an integer cell, also prevent the decimal point
          if (isInteger && (e.key === '.' || e.key === ','))
            e.preventDefault();
            
          // Enter = Save, Escape = Cancel
          if (e.key === 'Enter') $td.blur();
          else if (e.key === 'Escape') revertCell(cell);
        });
    
        // --- Update ---
        $input.off('blur').on('blur', function() {
          if(noDataChange(this.value, cell)) 
            setCellText(cell, cell.data());
          
          updateTableAndShiny(cell, this.value);
        });
      }, 500);
    });"
  
  factor_js <- 
    "// HANDLER FOR FACTOR COLUMNS
    table.on('dblclick', 'td.factor-edit-cell', function(e) {
      e.stopImmediatePropagation();
      
      var isEscaping = false;
      
      var $td = $(this);
      if ($td.find('select').length) return; // already editing
      
      var cell = table.cell(this);
      var colName = getColName(cell);
  
      if (factorInfo[colName]) {
        var choices = factorInfo[colName];
        var currentVal = cell.data();

        // Build the dropdown
        var $select = $('<select></select>').css('width', '100%');
        $.each(choices, function(i, value) {
          var $opt = $('<option></option>').val(value).text(value);
          if (value == currentVal) $opt.prop('selected', true);
          $select.append($opt);
        });
        
        // Add the dropdown to the cell
        $td.empty().append($select);
        $select.focus();
        
        $select.off('change blur').on('change blur', function(e) {
          if(isEscaping) return;
          if(noDataChange(this.value, cell))
            setCellText(cell, cell.data());
          
          updateTableAndShiny(cell, this.value);
        });
        
        $select.on('keydown', function(e) {
          if (e.key === 'Escape') {
            isEscaping = true;
            revertCell(cell);
          } else if (e.key === 'Enter') {
            $(this).blur(); // Trigger the change/blur event
          }
        });
      }
    }); /*end double-click*/"
  
  paste(main_js, numeric_js, factor_js, header_cb, "}")
  # paste(main_js, factor_js, header_cb, "}")
}
initialize_inline_edit_table_ui <- function(
    data, 
    column_defs = list(), 
    tableID, 
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
    has_double_header = FALSE,
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
  init_js <- get_init_js(factor_info$factor_levels, tableID, has_double_header, header_cb)
  
  callback_js <- glue::glue("
    {ifelse(callback_js != 'return table;', callback_js, '')}
    
    $(document).on('mouseenter', 'table.dataTable tbody tr', function() {{
      $(this).css('background-color', '{USER_ENTRY_BG_COLOR}');
    }});
    $(document).on('mouseenter', 'table.dataTable tbody td', function() {{
      $(this).css('cursor', 'pointer');
      $(this).attr('title', 'Double-click a cell to edit'); // Set tooltip
    }});
    $(document).on('mouseleave', 'table.dataTable tbody tr', function() {{
      $(this).css('background-color', 'inherit');
    }});
      
    // Start cell editing with Enter key (13)   
    table.on('key', function (e, datatable, key, cell, originalEvent) {{
      const ignoredKeys = [
        'Shift', 'Control', 'Alt', 'Meta',
        'Tab', 'Escape',
        'ArrowUp', 'ArrowDown', 'ArrowLeft', 'ArrowRight'
      ];
    
      // ignore non-character keys
      if (ignoredKeys.includes(e.key)) return;
    
      // already editing -> do nothing
      const $cell = $(cell.node());
      if ($cell.find('input, textarea, select').length > 0) return;
    
      // only trigger on printable keys (letters, numbers, symbols)
      if (!originalEvent || originalEvent.key.length !== 1) return;
    
      originalEvent.preventDefault();
    
      // open editor (same as your dblclick approach)
      $cell.trigger('dblclick.dt');
    
      // inject the typed character into the editor after it opens
      setTimeout(() => {{
        const input = $cell.find('input, textarea, select').first();
        if (input.length) input.val(originalEvent.key);
      }}, 0);
    }});
    
    // Exit cell editing with Tab (9), Enter (13), or Arrow Keys (37-40)
    table.on('keydown', function(e) {{
      var keys = [9,13,37,38,39,40];
      if(e.target.localName == 'input' && keys.indexOf(e.keyCode) > -1)
        $(e.target).trigger('blur');
    }});")
  
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
    editable = editable,
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

validate_numeric_entry <- function(df, col_name, val) {
  new_val <- ifelse(is.integer(df[[col_name]]), as.integer(val), as.numeric(val))
  
  max_val = fcase(
    grepl("BED", toupper(col_name)), 99999,
    toupper(col_name) == 'weighted_score', 100,
    grepl("FUNDING|AMOUNT", toupper(col_name)), 999999999
  )
  
  if ((!is.na(new_val) && (new_val < 0 || new_val > max_val)) || (is.na(new_val) && !is.na(val))) {
    showNotification(glue::glue("Invalid input: Please enter a number between 0 and {prettyNum(max_val, big.mark=',')}", type = "error"))
    return(FALSE)
  }
  
  return(TRUE)
}