$(document).on('mouseenter', 'table.dataTable tbody tr', function() {
  $(this).css('background-color', '{USER_ENTRY_BG_COLOR}');
});
$(document).on('mouseenter', 'table.dataTable tbody td', function() {
  $(this).css('cursor', 'pointer');
  $(this).attr('title', 'Double-click a cell to edit'); // Set tooltip
});
$(document).on('mouseleave', 'table.dataTable tbody tr', function() {
  $(this).find('td.focus').removeClass('focus');
  $(this).css('background-color', 'inherit');
  table.cell.blur();
});

$(document).on('click', 'table.dataTable tbody td', function() {
  $(this).addClass('focus');
});

const ignoredKeys = [
  'Shift', 'Control', 'Alt', 'Meta',
  'Tab', 'Escape',
  'ArrowUp', 'ArrowDown', 'ArrowLeft', 'ArrowRight'
];

// Start cell editing with Enter key (13)   
table.on('key', function (e, datatable, key, cell, originalEvent) {
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
  setTimeout(() => {
    const input = $cell.find('input, textarea, select').first();
    if (input.length) input.val(originalEvent.key);
  }, 0);
});

// Exit cell editing with Tab (9), Enter (13), or Arrow Keys (37-40)
table.on('keydown', function(e, datatable, key, cell, originalEvent) {
  // 9 = Tab, 13 = Enter, 27 = Escape, 37 = Left Arrow, 38 = Up Arrow, 39 = Right Arrow, 40 = Down Arrow
  var keys = [9,13,37,38,39,40];

  if(e.target.localName == 'input' && keys.includes(e.keyCode))
    $(e.target).trigger('blur');
  else if(e.key == 'Escape') {
    var cell = table.cell(e.target.parentNode);
    console.log(cell.data());
    revertCell(cell, table);
  }
});