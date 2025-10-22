// Documentation Navigation Handler
$(document).on('shiny:connected', function() {
  Shiny.addCustomMessageHandler('updateDocNav', function(message) {
    $('.doc-nav-item').removeClass('active');
    $(message.active).addClass('active');
  });
});
