// =============================================================================
// Loading Indicator JavaScript (Updated with better handling)
// =============================================================================

$(document).ready(function() {
  
  console.log("Loading indicator JavaScript loaded");
  
  // Handler to show loading overlay
  Shiny.addCustomMessageHandler('show_loading', function(message) {
    console.log('Show loading called for:', message.id);
    
    var $overlay = $('#' + message.id);
    
    if ($overlay.length) {
      console.log('Loading overlay found, showing...');
      // Add fade-in animation
      $overlay.addClass('fade-in show');
      
      // Remove fade-in class after animation
      setTimeout(function() {
        $overlay.removeClass('fade-in');
      }, 200);
    } else {
      console.warn('Loading overlay not found:', message.id);
    }
  });
  
  // Handler to hide loading overlay
  Shiny.addCustomMessageHandler('hide_loading', function(message) {
    console.log('Hide loading called for:', message.id);
    
    var $overlay = $('#' + message.id);
    
    if ($overlay.length) {
      console.log('Loading overlay found, hiding...');
      // Add fade-out animation
      $overlay.addClass('fade-out');
      
      // Remove show class after animation completes
      setTimeout(function() {
        $overlay.removeClass('show fade-out');
      }, 200);
    } else {
      console.warn('Loading overlay not found:', message.id);
    }
  });
  
});
