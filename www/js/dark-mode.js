$(document).on('shiny:connected', function() {
  if (!$('body').hasClass('dark-mode')) {
    $('#customSwitch1').click();
  }
});
