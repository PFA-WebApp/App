Translator2 <- R6::R6Class(
  classname = "Translator2",
  inherit = shiny.i18n::Translator,
  public = list(
    t = function(keyword) {
      super$t(keyword, session = NULL)
    }
  )
)
