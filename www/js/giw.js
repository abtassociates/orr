const panel = document.getElementById('inventory-giw_panel');
const handle = panel.querySelector('.giw-header');

let startX;
let startY;
let startLeft;
let startTop;
let initialized = false;

handle.addEventListener('mousedown', function(e) {

  // Don't drag when clicking the close button
  if ($(e.target).closest('button').length) {
    return;
  }

  // On the first drag, convert the centered position
  // into pixel coordinates.
  if (!initialized) {
    const rect = panel.getBoundingClientRect();

    panel.style.left = rect.left + 'px';
    panel.style.top = rect.top + 'px';
    panel.style.transform = 'none';

    initialized = true;
  }

  startX = e.clientX;
  startY = e.clientY;
  startLeft = panel.offsetLeft;
  startTop = panel.offsetTop;

  document.addEventListener('mousemove', drag);
  document.addEventListener('mouseup', stopDrag);

  e.preventDefault();
});

function drag(e) {
  const panelWidth = panel.offsetWidth;
  const panelHeight = panel.offsetHeight;

  const minLeft = 0;
  const minTop = 0;
  const maxLeft = Math.max(0, window.innerWidth - panelWidth);
  const maxTop = Math.max(0, window.innerHeight - panelHeight);

  const newLeft = startLeft + e.clientX - startX;
  const newTop = startTop + e.clientY - startY;

  panel.style.left =
    Math.max(minLeft, Math.min(newLeft, maxLeft)) + 'px';

  panel.style.top =
    Math.max(minTop, Math.min(newTop, maxTop)) + 'px';
}

function stopDrag() {
  document.removeEventListener('mousemove', drag);
  document.removeEventListener('mouseup', stopDrag);
}