// Dashboard Header
shinyjs.hideMainHeader = function(params) { $("body").find('.main-header').css( "display", "none" ); }

// Dashboard Box Header
shinyjs.hideBoxHeader = function(params) { $("body").find('.box-header').css( "display", "none" ); }

// Dashboard Sidebar
shinyjs.hideSidebar = function(params) { $("body").addClass("sidebar-collapse") }
shinyjs.showSidebar = function(params) { $("body").removeClass("sidebar-collapse") }
shinyjs.hideSidebarToggle = function(params) { $("a").removeClass("sidebar-toggle") }

// Dashboard Collection selection
shinyjs.hideinDselect = function(params) { $("body").find('.inDselect').css( "display", "none" ); }
shinyjs.showinDselect = function(params) { $("body").find('.inDselect').css( "display", "inline" ); }

// Dashboard Dataset subset selection
shinyjs.hideinDSselect = function(params) { $("body").find('.inDSselect').css( "display", "none" ); }
shinyjs.showinDSselect = function(params) { $("body").find('.inDSselect').css( "display", "inline" ); }


// Dashboard Tab selection
shinyjs.openTab = function(params)
{
   $('a', $('.sidebar')).each(function() {
      if(this.getAttribute('data-value') == params) { this.click() };
   });
}
