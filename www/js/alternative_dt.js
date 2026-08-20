var thead = $(table.header());

// We attach functions to 'window' so the HTML 'onclick' can find them
window.select_all_thresholds = function(e, val, colIdx, inputId) {
  // 1. STOP PROPAGATION: Prevents the header from sorting when the button is clicked
  if (e && e.stopPropagation) e.stopPropagation();
  if (e && e.stopImmediatePropagation) e.stopImmediatePropagation();
  if (e && e.preventDefault) e.preventDefault();
  
  // 1. Update Shiny
  Shiny.setInputValue(inputId, val, {priority: 'event'});
  
  var cellText = (val === 1) ? 'Yes' : 'No';

  // 2. Update the internal DataTables model for all rows in this column
  // This ensures sorting/filtering doesn't revert the values
  table.column(colIdx).data().each(function(d, i) {
    table.cell(i, colIdx).data(cellText);
  });
};

window.create_select_all_btns = function(th, title, inputId) {
  var colIdx = table.column(th).index();
  
  // Note: We wrap inputId in single quotes within the onclick string
  $(th).html(
    `${title}<div style='margin-top:4px; white-space:nowrap;'>
      <button class='btn btn-xs btn-success' style='margin-right:2px;'
        onclick=\"window.select_all_thresholds(event, 1, ${colIdx}, '${inputId}')\">✓ All</button>
      <button class='btn btn-xs btn-danger'
        onclick=\"window.select_all_thresholds(event, 2, ${colIdx}, '${inputId}')\">✗ None</button>
      </div>
   `
  );
};

// Attach select all+none buttons
thead.find('th').each(function() {
  var colName = $(this).text().trim();
debugger;
  if (colName === 'Met HUD Thresholds')
      create_select_all_btns(this, 'MET HUD THRESHOLDS', '__MET_HUD_INPUT_ID__')
  
  if (colName === 'Met CoC Thresholds')
    create_select_all_btns(this, 'MET COC THRESHOLDS', '__MET_COC_INPUT_ID__')
});
