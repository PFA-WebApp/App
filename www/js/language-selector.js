var binding = new Shiny.InputBinding();

$.extend(binding, {

  find: function(scope) {
    return $(scope).find(".language-selector");
  },

  initialize: function(el) {
    let selected = $(el).attr("data-selected");
    $(el).data("language", selected);

    $(el).find(".dropdown-toggle").text(this.flagsDict[selected]);

    let choices = $(el).attr("data-choices").split(",");
    let $menu = $(el).find(".dropdown-menu");
    for (const choice of choices) {
      $menu.append(
        $(
          '<span class="dropdown-item dropdown-flag" data-key="'
          + choice
          + '">'
          + this.flagsDict[choice]
          + '</span>'
        )
      );
    }
  },

  getValue: function(el) {
    return $(el).data("language");
  },

  setValue: function(el, data) {
    $(el).data("language", data.language);
    $(el).find(".dropdown-toggle").text(this.flagsDict[data.language]);

    $(el).trigger("change");
  },

  receiveMessage: function(el, data) {
    this.setValue(el, data);
  },

  subscribe: function(el, callback) {
    var that = this;
    $(el).on("click.language-selector", ".dropdown-item", function(e) {
      that.setValue(el, {language: $(this).attr("data-key")});
      callback();
    });
  },

  unsubscribe: function(el) {
    $(el).off(".language-selector");
  },

  flagsDict: {
    de: "\u{1F1E9}\u{1F1EA}",
    en: "\u{1F1EC}\u{1F1E7}",
    fr: "\u{1F1EB}\u{1F1F7}"
  }

});

Shiny.inputBindings.register(binding, "language-selector");
