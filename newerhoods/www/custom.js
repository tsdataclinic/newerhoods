$(document).ready(function() {
  setTimeout(function() {
    shinyBS.addTooltip("select", "tooltip", {
      container: "body",
      placement: "right",
      trigger: "hover",
      title: "Click to select or update features to be used for clustering"
    });
  }, 500);
});

$(function() {
  $('[data-toggle="tooltip"]').tooltip();
});

$(function() {
  $('[data-toggle="popover"]').popover();
});


