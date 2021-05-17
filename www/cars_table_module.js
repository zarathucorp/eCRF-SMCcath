function cars_table_module_js(ns_prefix) {

  $("#" + ns_prefix + "car_table").on("click", ".delete_btn", function() {
    Shiny.setInputValue(ns_prefix + "car_id_to_delete", this.id, { priority: "event"});
    $(this).tooltip('hide');
  });
  
  // demographic edit button
  $("#" + ns_prefix + "car_table").on("click", ".edits_btn", function() {
    Shiny.setInputValue(ns_prefix + "car_id_to_edit", this.id, { priority: "event"});
    $(this).tooltip('hide');
  });
  
  // admission edit button
  $("#" + ns_prefix + "car_table").on("click", ".edit_btnadm", function() {
    Shiny.setInputValue(ns_prefix + "car_id_to_edit_adm", this.id, { priority: "event"});
    $(this).tooltip('hide');
  });
  
  // angiographics edit button
  $("#" + ns_prefix + "car_table").on("click", ".edit_btnang", function() {
    Shiny.setInputValue(ns_prefix + "car_id_to_edit_ang", this.id, { priority: "event"});
    $(this).tooltip('hide');
  });
  
  // culprit1 edit button
  $("#" + ns_prefix + "car_table").on("click", ".edit_btncul1", function() {
    Shiny.setInputValue(ns_prefix + "car_id_to_edit_cul1", this.id, { priority: "event"});
    $(this).tooltip('hide');
  });
  
  // culprit2 edit button
  $("#" + ns_prefix + "car_table").on("click", ".edit_btncul2", function() {
    Shiny.setInputValue(ns_prefix + "car_id_to_edit_cul2", this.id, { priority: "event"});
    $(this).tooltip('hide');
  });

  // Non-culprit1 edit button
  $("#" + ns_prefix + "car_table").on("click", ".edit_btnculn1", function() {
    Shiny.setInputValue(ns_prefix + "car_id_to_edit_culn1", this.id, { priority: "event"});
    $(this).tooltip('hide');
  });

  // Non-culprit2 edit button
  $("#" + ns_prefix + "car_table").on("click", ".edit_btnculn2", function() {
    Shiny.setInputValue(ns_prefix + "car_id_to_edit_culn2", this.id, { priority: "event"});
    $(this).tooltip('hide');
  });
  
  // Non-culprit3 edit button
  $("#" + ns_prefix + "car_table").on("click", ".edit_btnculn3", function() {
    Shiny.setInputValue(ns_prefix + "car_id_to_edit_culn3", this.id, { priority: "event"});
    $(this).tooltip('hide');
  });
  
  // Non-culprit4 edit button
  $("#" + ns_prefix + "car_table").on("click", ".edit_btnculn4", function() {
    Shiny.setInputValue(ns_prefix + "car_id_to_edit_culn4", this.id, { priority: "event"});
    $(this).tooltip('hide');
  });
  
  // outcomes edit button
  $("#" + ns_prefix + "car_table").on("click", ".edit_btnoutc", function() {
    Shiny.setInputValue(ns_prefix + "car_id_to_edit_outc", this.id, { priority: "event"});
    $(this).tooltip('hide');
  });
  

  // event edit button
  // $("#" + ns_prefix + "car_table").on("click", ".edit_btn", function() {
  //  Shiny.setInputValue(ns_prefix + "car_id_to_edit_event", this.id, { priority: "event"});
  //  $(this).tooltip('hide');
  // });
  
  // lab edit button
  // $("#" + ns_prefix + "car_table").on("click", ".editl_btn", function() {
  //   Shiny.setInputValue(ns_prefix + "car_id_to_edit_lab", this.id, { priority: "event"});
  //   $(this).tooltip('hide');
  // });
  
  // m1 edit button
  $("#" + ns_prefix + "car_table").on("click", ".editm1_btn", function() {
    Shiny.setInputValue(ns_prefix + "car_id_to_edit_m1", this.id, { priority: "event"});
    $(this).tooltip('hide');
  });
  
  // m3 edit button
  $("#" + ns_prefix + "car_table").on("click", ".editm3_btn", function() {
    Shiny.setInputValue(ns_prefix + "car_id_to_edit_m3", this.id, { priority: "event"});
    $(this).tooltip('hide');
  });
  
  // m6 edit button
  $("#" + ns_prefix + "car_table").on("click", ".editm6_btn", function() {
    Shiny.setInputValue(ns_prefix + "car_id_to_edit_m6", this.id, { priority: "event"});
    $(this).tooltip('hide');
  });
  
  // mf edit button
  $("#" + ns_prefix + "car_table").on("click", ".editmf_btn", function() {
    Shiny.setInputValue(ns_prefix + "car_id_to_edit_mf", this.id, { priority: "event"});
    $(this).tooltip('hide');
  });
}


