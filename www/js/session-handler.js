// =============================================================================
// session-handler.js - Handle Shiny Session Disconnection
// =============================================================================

$(document).on('shiny:disconnected', function(event) {
  // Create overlay
  const overlay = document.createElement('div');
  overlay.id = 'session-ended-overlay';
  overlay.innerHTML = `
    <div class="session-ended-container">
      <div class="session-ended-icon">
        <i class="fas fa-plug-circle-xmark"></i>
      </div>
      <h2 class="session-ended-title">Session Ended</h2>
      <p class="session-ended-message">
        Your session has ended due to inactivity or disconnection.
      </p>
      <button class="session-reload-btn" onclick="location.reload()">
        <i class="fas fa-rotate-right"></i> Reload Application
      </button>
    </div>
  `;
  
  document.body.appendChild(overlay);
});

// Optional: Show warning before timeout
let inactivityTime = function () {
  let time;
  
  // Reset timer on user activity
  window.onload = resetTimer;
  document.onmousemove = resetTimer;
  document.onkeypress = resetTimer;
  document.onclick = resetTimer;
  document.onscroll = resetTimer;
  
  function showWarning() {
    // Show a warning 2 minutes before timeout
    Shiny.setInputValue('session_warning', true, {priority: 'event'});
  }
  
  function resetTimer() {
    clearTimeout(time);
    // Set to 28 minutes (if timeout is 30 min)
    time = setTimeout(showWarning, 28 * 60 * 1000);
  }
};

// Initialize inactivity timer
window.addEventListener('load', function() {
  inactivityTime();
});
