modifyFileInputText = function() {
  $('.shiny-file-input-progress').on("DOMSubtreeModified", function() {

    var target = $('.shiny-file-input-progress').children()[0];
    if (target.innerHTML === "Upload complete") {
        target.innerHTML = 'Hochladen abgeschlossen';
    }
  });
};

modifyFileInputLabel = function() {
  $("[for='container-file_management-file_manager_group-upload']").parent().on("DOMSubtreeModified", function() {
    filesText = $("[for='container-file_management-file_manager_group-upload']").
    parent().find("input").eq(1).val();
    newFilesText = filesText.replace("files", "Dateien");
    $("[for='container-file_management-file_manager_group-upload']").
    parent().find("input").eq(1).val(newFilesText);
  });
};

$(modifyFileInputText);
$(modifyFileInputLabel);
