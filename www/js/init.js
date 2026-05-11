function(settings, json) {
  var table = this.api();
  var factorInfo = __FACTOR_INFO__;
  
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
      var colName = getColName(cell, table);
      if (!factorInfo[colName].includes(pastedValue)) return;
    }
    
    // Don't process unchanged data
    if(noDataChange(pastedValue, cell)) return;
    
    if($td.hasClass('numeric-edit-cell')) pastedValue = getRawVal(pastedValue);
    
    updateTableAndShiny(cell, table, pastedValue);
  }, true);
  
  /****************
   * NUMERIC HANDLING
   *****************/
  table.on('dblclick', 'td.numeric-edit-cell', function(e) {
    //e.stopImmediatePropagation();
    var $td = $(this);
    setTimeout(function() {
      //if ($td.find('input').length) return; // already editing
  
      var cell = table.cell($td);
      var colName = getColName(cell, table).toUpperCase();

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
          
        debugger;
        // Enter = Save, Escape = Cancel
        if (e.key === 'Enter') $td.blur();
        else if (e.key === 'Escape') revertCell(cell, table);
      });
  
      // --- Update ---
      $input.off('blur').on('blur', function() {
        if(noDataChange(this.value, cell)) 
          setCellText(cell, cell.data());
        
        updateTableAndShiny(cell, table, this.value);
      });
    }, 500);
  });
  
  /**********************
   * FACTOR HANDLER
   *********************/
  table.on('dblclick', 'td.factor-edit-cell', function(e) {
    e.stopImmediatePropagation();
    
    var isEscaping = false;
    
    var $td = $(this);
    if ($td.find('select').length) return; // already editing
    
    var cell = table.cell(this);
    var colName = getColName(cell, table);

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
        
        updateTableAndShiny(cell, table, this.value);
      });
      
      $select.on('keydown', function(e) {
        if (e.key === 'Escape') {
          isEscaping = true;
          revertCell(cell, table);
        } else if (e.key === 'Enter') {
          $(this).blur(); // Trigger the change/blur event
        }
      });
    }
  }); /*end double-click*/
  
  /****************
   * OTHER DATA TYPE
   *****************/
  table.on('dblclick', 'td:not(.numeric-edit-cell, .factor-edit-cell)', function(e) {
    //e.stopImmediatePropagation();
    var $td = $(this);
    setTimeout(function() {
      var cell = table.cell($td);
      var colName = getColName(cell, table).toUpperCase();

      // --- 1. Get the input ---
      var $input = $td.find('input[type=text]');
      
      // --- 2. Enforce the Character Limit! ---
      $input.off('input').on('input', function() {
        var val = this.value;
        this.value = trim_val(colName, val);
      });
      
      $input.off('keydown').on('keydown', function(e) {
        // Enter = Save, Escape = Cancel
        if (e.key === 'Enter') $td.blur();
        else if (e.key === 'Escape') revertCell(cell, table);
      });
  
      // --- Update ---
      $input.off('blur').on('blur', function() {
        if(noDataChange(this.value, cell)) 
          setCellText(cell, cell.data());
        
        if (colName === "GEO CODE" && !this.value.startsWith("#"))
          this.value = `#${this.value}`;
        updateTableAndShiny(cell, table, this.value);
      });
    }, 500);
  });
  
  __HEADER_CB__
}