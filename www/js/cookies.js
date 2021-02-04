shinyjs.getCookie = function(params) {
  var cookie = Cookies.get(params.cookie);
  Shiny.setInputValue(params.id, cookie);
};

shinyjs.setCookie = function(params) {
  Cookies.set(params.cookie, params.value, {
    expires: 1/1440,
    sameSite: 'strict'
  });
  Shiny.setInputValue(params.id, params.value);
};

shinyjs.rmCookie = function(params) {
  Cookies.remove(params.cookie);
  Shiny.setInputValue(params.id, undefined);
};
