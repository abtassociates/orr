function getCompoundColName(colIndex, table) {
  var $thead = $(table.table().header());
  var headerRows = $thead.find('tr');
  
  if (headerRows.length < 2) {
    return $(headerRows[0]).find('th').eq(colIndex).text().trim();
  }
  
  var topCells = $(headerRows[0]).find('th');
  var rowspanOffset = 0;
  var colCursor = 0;
  var groupName = '';
  
  topCells.each(function() {
    var span = parseInt($(this).attr('colspan') || 1);
    var rowspan = parseInt($(this).attr('rowspan') || 1);
    
    if (colIndex >= colCursor && colIndex < colCursor + span) {
      groupName = $(this).text().trim();
      return false;
    }
    if (rowspan > 1) rowspanOffset += span;
    colCursor += span;
  });
  
  var bottomRowIndex = colIndex - rowspanOffset;
  var subName = $(headerRows[1]).find('th').eq(bottomRowIndex).text().trim();
  
  return groupName && groupName !== subName
  ? groupName.toUpperCase() + '_' + subName.toLowerCase()
  : subName.toLowerCase();
}

function getShinyTableId(table) {
  return(
    $(table.table().node()).closest('.datatables').attr('id')
  )
}
function getColName(cell, table) {
  var colIndex = cell.index().column;
  
  var hasMultiHeaders = table.header()[0].querySelector('tr.multi-header-row') !== null;

  let colName = table.column(colIndex).header().innerText.toUpperCase();
  if (hasMultiHeaders) {
    colName = getCompoundColName(colIndex, table);
  } else if (getShinyTableId(table) == 'rating-alternative-alternative_rating_table') {
    colName = table.column(colIndex).header().childNodes[0].wholeText;
  }
  return colName.replace(' Ⓘ','');
}

function is_funding_col(c) {
  if(c == 'FUNDING ACTION') return false;
  return c.includes('FUNDING') || c.includes('AMOUNT');
}

function formatUSD(amount) {
  if(amount === null) return;
  amount = Number(amount);
  if (typeof amount !== 'number' || isNaN(amount)) {
    throw new Error('Invalid input: amount must be a valid number.');
  }
  return new Intl.NumberFormat('en-US', {
    style: 'currency', currency: 'USD',
    minimumFractionDigits: 0, maximumFractionDigits: 0
  }).format(amount);
}

function getFormattedVal(colName, val) {
  return is_funding_col(colName.toUpperCase()) ? formatUSD(val) : val.toLocaleString('en-US');
}

function setCellText(cell, val, table) {
  var $td = $(cell.node());
  let formatted_val = getFormattedVal(getColName(cell, table), val);
  $td.empty().text(formatted_val); 
}

function revertCell(cell, table) {
  setCellText(cell, cell.data(), table);
}

function getRawVal(val) {
  if(val === null) return val;
  return parseFloat(val.toString().replace(/[^\\d.]/g, ''));
}

function noDataChange(val, cell) {
  return getRawVal(val) == getRawVal(cell.data());
}

function get_max_length(colName) {
  let c = colName.toUpperCase();
  if(c.includes('BED')) return 5;
  if(c == 'WEIGHTED_SCORE') return 3;
  if(is_funding_col(c)) return 10;
  if(c == "GEO CODE") return 9; /*# the actual max is 10 but a "#" is prepended AFTER this check*/
  return null;
}

function trim_val(colName, val, max_length = null) {
  let maxLength = max_length ? max_length : get_max_length(colName);
  if (maxLength && val.length > maxLength) val = val.slice(0, maxLength);
  return val;
}

function isNumeric(str) {
  str = str.replace(/[^\d.]/g, '');
  if (typeof str !== 'string' || str.trim() === '') return false;
  return !Number.isNaN(Number(str));
}

// If user makes a change, we update in several ways:
// 1. set the text of the cell to whatever dropdown value they select
// 2. trigger a cell_edit event on the server, which will:
    // 1. update the underlying datatable data (so that the new value will be preserved in sorting and filtering)
    // 2. update the database
// 3. set the cell data to the same value. This is so if they double-click the cell again, the new value will appear as the default selection
function updateTableAndShiny(cell, table, val) {
  var colName = getColName(cell, table).toUpperCase();
  setCellText(cell, val, table);
  alertShiny(cell, val, table);
  cell.data(val);
}

function alertShiny(cell, val, table) {
  Shiny.setInputValue(getShinyTableId(table) + '_cell_edit', {
    row: cell.index().row + 1,
    col: cell.index().column,
    value: val,
    oldValue: cell.data(),
    project_id: table.cells(cell.index().row, 0).data()[0],
  }, {priority: 'event'});
}