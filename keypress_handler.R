keypress_handler <- 
  '$(document).on("shiny:value", function(e) {
     if (e.name == "trace_plot") {
       Shiny.setInputValue("keypress", 0);
     }
   });
            
   $(document).on("keypress", function (e) {
     if (["1", "2", "3"].includes(e.key)) {
       Shiny.setInputValue("keypress", e.key);
     }
   });'