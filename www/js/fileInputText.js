modifyFileInputText = function() {
  $('.shiny-file-input-progress').on("DOMSubtreeModified", function() {
    target = $('.shiny-file-input-progress').each(function() {
      target = $(this).children()[0];
      if (target.innerHTML === "Upload complete") {
        target.innerHTML = 'Hochladen abgeschlossen';
      }
    });
  });
};

modifyFileInputLabel = function() {
  $(".shiny-file-input-progress").parent().on("DOMSubtreeModified", function() {
    label = $(".shiny-file-input-progress");
    label.each(function() {
      text = $(this).parent().find("input").eq(1).val();
      newText = text.replace("files", "Dateien");
      $(this).parent().find("input").eq(1).val(newText);
    });
  });
};

$(modifyFileInputText);
$(modifyFileInputLabel);
