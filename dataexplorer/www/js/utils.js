// Get a PDF through the API
// apikey, ipclient : global variables
var openPDF = function(url) {
   $.ajax({
     url: url,
     cache: false,
     xhrFields: { responseType: 'blob' },
     headers: { "x-api-key": apikey, 'x-forwarded-for': ipclient },
     success: function(blob) {
       var link=document.createElement('a');
       link.href=window.URL.createObjectURL(blob);
       link.type = 'application/pdf';
       link.target='_blank';
       link.click();
     }
   });
}

// Get XML through the API
// apikey, ipclient : global variables
var openXML = function(url) {
   $.ajax({
     url: url,
     cache: false,
     dataType: 'xml',
     headers: { "x-api-key": apikey, 'x-forwarded-for': ipclient },
     success: function(response) {
        var xml = response;

        //extract the stylesheet so we can load it manually
        var stylesheet;
        for (var i=0;i<xml.childNodes.length;i++)
            if ( xml.childNodes[i].nodeName =='xml-stylesheet' )
                stylesheet = xml.childNodes[i].data;
        var items = stylesheet.split('=');
        var xsltFile = items[items.length-1].replace(/"/g,'');

        //fetch xslt manually
        $.get( xsltFile, function(xslt){
            var transformed;
            if (! window['XSLTProcessor']) {
                // Trasformation for IE
                transformed = xml.transformNode(xslt);
            } else {
                // Transformation for non-IE
                var processor = new XSLTProcessor();
                processor.importStylesheet(xslt);
                var xmldom = processor.transformToDocument(xml);
                var serializer = new XMLSerializer();
                transformed = serializer.serializeToString(xmldom.documentElement);
            }

            var newwindow = window.open();
            newwindow.document.open();
            newwindow.document.write(transformed);
            newwindow.document.close();
            newwindow.location.href=url;
            newwindow.document.title = url;
        });
     }
   });
}


// Toggle input of the API Key, from 'password' to 'text' and vice-versa
var eyetoggle = function() {
   if ( $('#eyeapikey').attr('src') === 'eye-close.png' ) {
      $('#eyeapikey').attr('src','eye-open.png');
      $('#authkey').get(0).type = 'text';
   } else {
      $('#eyeapikey').attr('src','eye-close.png');
      $('#authkey').get(0).type = 'password';
   }
}

// Send IP to Shiny Server 
var sendIP = function(theip) {
   Shiny.onInputChange("ipclient", theip);
   console.log(theip);
}

// Convert data to JSON 
var toJSON = function( data ) {
   json = data.trim().split('\n').reduce(function(obj, pair) {
       pair = pair.split('=');
       return obj[pair[0]] = pair[1], obj;
   },{});
   return json;
}

// Get IP Client
// https://stackoverflow.com/questions/391979/how-to-get-clients-ip-address-using-javascript

// When the shiny session is initialized
// => Get IP Client, then send it to Shiny Server 
$( document ).on("shiny:sessioninitialized", function(event) {

   $.ajax({
// cloudflare.com
//     url: 'https://www.cloudflare.com/cdn-cgi/trace',
// bigdatacloud.net
//     url: 'https://api.bigdatacloud.net/data/ip-geolocation?key=b43709c79a3f404682eb58d00ed95f17',
// ipify.org
     url: 'https://api.ipify.org/?format=json',
     timeout: 5000
   }).done(function(data) {
// cloudflare.com
//     data = toJSON( data )
// bigdatacloud.net
//       if(data.responseJSON.hasOwnProperty('ip'))
//          sendIP( data.responseJSON["ip"] );
       if(data.hasOwnProperty('ip'))
          sendIP( data["ip"] );
       else 
          sendIP( "127.0.0.1" );
   }).fail(function( jqXHR, textStatus ) {
         sendIP( "127.0.0.1" );
   });

})

