// =============================================================================
// LANDING PAGE CAROUSEL - Complete Implementation
// =============================================================================

function initInnerCarousels() {
  console.log('Initializing inner carousels...');
  
  function cycleInnerCarousel(id, interval) {
    var container = document.getElementById(id);
    if (!container) {
      console.error('❌ Container not found:', id);
      return;
    }
    
    var slides = container.querySelectorAll('.inner-slide');
    if (!slides.length) {
      console.error('❌ No slides found in:', id);
      return;
    }
    
    console.log('✅ Found', slides.length, 'slides in', id);
    
    // Ensure first slide is visible
    slides.forEach(function(s) { s.classList.remove('active'); });
    slides[0].classList.add('active');
    
    var index = 0;
    setInterval(function() {
      slides[index].classList.remove('active');
      index = (index + 1) % slides.length;
      slides[index].classList.add('active');
    }, interval);
  }
  
  cycleInnerCarousel('rob-inner-carousel', 4000);
  cycleInnerCarousel('meta-inner-carousel', 4000);
  cycleInnerCarousel('nma-inner-carousel', 4000);
}

// =============================================================================
// MAIN CAROUSEL NAVIGATION
// =============================================================================

function initMainCarousel() {
  console.log('Initializing main carousel...');
  
  var slides = document.querySelectorAll('.carousel-slide');
  var indicators = document.querySelectorAll('.carousel-indicator');
  var prevBtn = document.getElementById('carousel-prev');
  var nextBtn = document.getElementById('carousel-next');
  
  if (!slides.length) {
    console.error('❌ No carousel slides found');
    return;
  }
  
  console.log('✅ Found', slides.length, 'main slides');
  
  var currentSlide = 0;
  
  function showSlide(n) {
    slides.forEach(function(s) { s.classList.remove('active'); });
    indicators.forEach(function(i) { i.classList.remove('active'); });
    
    currentSlide = (n + slides.length) % slides.length;
    slides[currentSlide].classList.add('active');
    if (indicators[currentSlide]) {
      indicators[currentSlide].classList.add('active');
    }
  }
  
  // Previous slide
  if (prevBtn) {
    prevBtn.addEventListener('click', function() {
      showSlide(currentSlide - 1);
    });
  }
  
  // Next slide
  if (nextBtn) {
    nextBtn.addEventListener('click', function() {
      showSlide(currentSlide + 1);
    });
  }
  
  // Indicator clicks
  indicators.forEach(function(indicator, idx) {
    indicator.addEventListener('click', function() {
      showSlide(idx);
    });
  });
  
  // Initialize first slide
  showSlide(0);
}

// =============================================================================
// SHINY INITIALIZATION - with delay for DOM readiness
// =============================================================================

$(document).on('shiny:connected', function() {
  console.log('🔗 Shiny connected');
  
  // Wait for DOM to fully render
  setTimeout(function() {
    console.log('⏱️  Starting carousel initialization...');
    initMainCarousel();
    initInnerCarousels();
    console.log('✅ All initialization complete');
  }, 250);
});
