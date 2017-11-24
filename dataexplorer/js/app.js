shinyjs.hideSidebar = function(params) { $("body").addClass("sidebar-collapse") }
shinyjs.showSidebar = function(params) { $("body").removeClass("sidebar-collapse") }
shinyjs.hideSidebarToggle = function(params) { $("a").removeClass("sidebar-toggle") }

shinyjs.hideMainHeader = function(params) { $("body").find('.main-header').css( "display", "none" ); }

shinyjs.hideinDSelect = function(params) { $("body").find('.inDSelect').css( "display", "none" ); }
shinyjs.showinDSelect = function(params) { $("body").find('.inDSelect').css( "display", "inline" ); }

shinyjs.openTab = function(params){
     $('a', $('.sidebar')).each(function() {
        if(this.getAttribute('data-value') == params) { this.click() };
     });
}