// Get IP Client
// https://stackoverflow.com/questions/391979/how-to-get-clients-ip-address-using-javascript

var sendIP = function(theip) {
   Shiny.onInputChange("ipclient", theip);
   console.log(theip);
}

var toJSON = function( data ) {
    json = data.trim().split('\n').reduce(function(obj, pair) {
         pair = pair.split('=');
         return obj[pair[0]] = pair[1], obj;
    },{});
    return json;
}

$( document ).on("shiny:sessioninitialized", function(event) {

   $.ajax({
// cloudflare.com
     url: 'https://www.cloudflare.com/cdn-cgi/trace',
// ipify.org
//     url: 'https://api.ipify.org/?format=json',
// bigdatacloud.net
//     url: 'https://api.bigdatacloud.net/data/ip-geolocation?key=b43709c79a3f404682eb58d00ed95f17',
     timeout: 5000
   }).done(function(data) {
// cloudflare.com
     data = toJSON( data )
// bigdatacloud.net
//      if(data.responseJSON.hasOwnProperty('ip'))
//         sendIP( data.responseJSON["ip"] );
      if(data.hasOwnProperty('ip'))
         sendIP( data["ip"] );
      else 
         sendIP( "127.0.0.1" );
   }).fail(function( jqXHR, textStatus ) {
         sendIP( "127.0.0.1" );
   });

})
