keypress_handler <- 
  '$(document).on("keypress", function (e) {
     if (["1", "2", "3", "d"].includes(e.key)) {
       Shiny.setInputValue("keypress", e.key);
       setTimeout(function(){Shiny.setInputValue("keypress", 0)}, 500)
     }
   });'