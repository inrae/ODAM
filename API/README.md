![Logo](swagger_logo.png)

# Swagger UI

You can test your getData API through the web swagger UI. For that, launch the run script with parameters corresponding to your local implementation, namely the API_URL (default is http://www.bordeaux.inra.fr/pmb/api/ODAM/1.0.0/odam-oas3.yaml) and the PORT (default is 8084).
```
    $ sh ./run -p 8081 -a http://www.bordeaux.inra.fr/pmb/api/ODAM/1.0.0/odam-oas3.yaml
```
Then, in your Web browser, you can launch the swagger UI with the URL http://localhost:8081/ (depending on the PORT setting).

For more information See [Github Swagger-UI](https://github.com/swagger-api/swagger-ui)

* [![](https://images.microbadger.com/badges/version/swaggerapi/swagger-ui.svg)](https://microbadger.com/images/swaggerapi/swagger-ui "Get your own version badge on microbadger.com")
[![](https://images.microbadger.com/badges/image/odam/swagger-ui-customized.svg)](https://microbadger.com/images/odam/swagger-ui-customized "Get your own image badge on microbadger.com")