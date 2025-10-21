// =============================================================================
// Banter Loader - Dynamically creates animated grid loader
// =============================================================================

$(document).ready(function() {
  console.log("Banter loader initialized");
  
  // Create the loader HTML structure dynamically
  function createBanterLoader() {
    var loaderHTML = '<div id="app-startup-loader">' +
      '<div class="startup-content">' +
      '<div class="banter-loader">';
    
    // Create 9 boxes
    for (var i = 0; i < 9; i++) {
      loaderHTML += '<div class="banter-loader__box"></div>';
    }
    
    loaderHTML += '</div>' +
      '<div class="startup-text">Loading MetaSuite</div>' +  // ← CHANGED
      '<div class="startup-subtext">Preparing your workspace...</div>' +
      '</div>' +
      '</div>';
    
    // Append to body
    $('body').prepend(loaderHTML);
  }
  
  // Create loader immediately
  createBanterLoader();
  
  // Hide loader when Shiny is fully loaded
  $(document).on('shiny:idle', function(event) {
    setTimeout(function() {
      $('#app-startup-loader').addClass('hide');
      // Remove from DOM after transition
      setTimeout(function() {
        $('#app-startup-loader').remove();
      }, 500);
    }, 1200);  // Show loader for at least 1.2 seconds
  });
});
