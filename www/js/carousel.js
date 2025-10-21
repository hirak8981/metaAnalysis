// =============================================================================
// Carousel Navigation & Tab Switching
// =============================================================================

$(document).ready(function() {
  
  let currentSlide = 0;
  const slides = $('.carousel-slide');
  const indicators = $('.carousel-indicator');
  const totalSlides = slides.length;
  
  // Show specific slide
  function showSlide(index) {
    slides.removeClass('active');
    indicators.removeClass('active');
    
    $(slides[index]).addClass('active');
    $(indicators[index]).addClass('active');
    
    currentSlide = index;
    
    // Update button states
    $('#carousel-prev').prop('disabled', index === 0);
    $('#carousel-next').prop('disabled', index === totalSlides - 1);
  }
  
  // Previous slide
  $(document).on('click', '#carousel-prev', function() {
    if (currentSlide > 0) {
      showSlide(currentSlide - 1);
    }
  });
  
  // Next slide
  $(document).on('click', '#carousel-next', function() {
    if (currentSlide < totalSlides - 1) {
      showSlide(currentSlide + 1);
    }
  });
  
  // Indicator clicks
  indicators.on('click', function() {
    const index = $(this).data('slide');
    showSlide(index);
  });
  
  // =============================================================================
  // TAB NAVIGATION - Using Shiny Input Values
  // =============================================================================
  
  // Function to navigate to a tab
  function navigateToTab(tabName) {
    console.log('Navigating to:', tabName);
    
    // Find the nav link and click it
    $('.nav-link').each(function() {
      let dataValue = $(this).attr('data-value');
      if (dataValue === tabName) {
        $(this).click();
        // Scroll to top
        window.scrollTo(0, 0);
        return false;
      }
    });
  }
  
  // Navigate from carousel buttons
  $(document).on('click', 'button[id$="goto_rob"], button[id*="get_started"]', function(e) {
    console.log('ROB button clicked');
    navigateToTab('rob_tab');
  });
  
  $(document).on('click', 'button[id$="goto_meta"]', function(e) {
    console.log('Meta button clicked');
    navigateToTab('meta_tab');
  });
  
  $(document).on('click', 'button[id$="goto_network"]', function(e) {
    console.log('Network button clicked');
    navigateToTab('network_tab');
  });
  
  // Initialize first slide
  showSlide(0);
  
  console.log('Carousel initialized');
});
