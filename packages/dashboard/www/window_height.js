// Define function to set height of "map" and "map_container"
setHeight = function() {
  var window_height = $(window).height();
  var header_height = $(".main-header").height();
  var requests_height = $("#requests").height();
  var boxHeight = window_height - header_height - requests_height - 30;
  //var areafocusHeight = window_height - header_height - requests_height - 30;
  var areafoucustitleHeight = $("#title_focus_list").height();
  var areafocusoptionsHeight = $("#top10options").height();
  // size of list we want is areafoucs height - title and optins height 
  var areafocusList = boxHeight - (areafoucustitleHeight + areafocusoptionsHeight);

  console.log(boxHeight);
  //console.log(areafoucustitleHeight);
  console.log(areafocusoptionsHeight);
  console.log(areafocusList);
  console.log(boxHeight - 20);
  console.log(areafocusList - 20);

  $("#map").height(boxHeight - 20);
  $("#areas2focus_list").height(areafocusList - 20);
  $(".nav-tabs-custom>.tab-content").height(boxHeight - 20);

};

// Set input$box_height when the connection is established
$(document).on("shiny:connected", function(event) {
  setHeight();
});

 // Refresh the box height on every window resize event    
$(window).on("resize", function(){
  setHeight();
});