// Get IP Client
// https://stackoverflow.com/questions/391979/how-to-get-clients-ip-address-using-javascript

$( document ).on("shiny:sessioninitialized", function(event) {
//   var apiKey = "b43709c79a3f404682eb58d00ed95f17";
//   $.ajaxSetup({ async: false });
//     resp=$.getJSON("https://api.bigdatacloud.net/data/ip-geolocation?key=" + apiKey);
//   $.ajaxSetup({ async: true  });
//   if(resp.responseJSON.hasOwnProperty('ip'))
//      theip = resp.responseJSON["ip"];
//   else
//      theip = "127.0.0.1";
//   Shiny.onInputChange("ipclient", theip);
//   console.log(theip);

   $.get('https://www.cloudflare.com/cdn-cgi/trace', function(data) {
      data = data.trim().split('\n').reduce(function(obj, pair) {
         pair = pair.split('=');
         return obj[pair[0]] = pair[1], obj;
      }, {});
      if(data.hasOwnProperty('ip'))
         theip = data["ip"];
      else 
         theip = "127.0.0.1";
      Shiny.onInputChange("ipclient", theip);
      console.log(theip);
   });
})
