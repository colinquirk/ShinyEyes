keypress_handler <- 
  '$(document).on("shiny:value", function(e) {
     if(e.name == "trace_plot") {
       Shiny.setInputValue("keypress", 0);
     }
   });
            
   $(document).on("keypress", function (e) {
     Shiny.setInputValue("keypress", e.key);
   });'