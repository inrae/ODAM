![Logo](swagger.png)

# Swagger UI

You can also test your getData API through the web swagger UI. For that, launch the run script with parameters corresponding to your local implementation, namely the API_URL (default is http://www.bordeaux.inra.fr/pmb/api/ODAM/1.0.0/odam-oas3.yaml) and the PORT (default is 8084).
```
    $ sh ./run -p 8080 -a http://www.bordeaux.inra.fr/pmb/api/ODAM/1.0.0/odam-oas3.yaml
```
Then, in your Web browser, you can launch the swagger UI with the URL http://localhost:8080/ (depending on the PORT setting).

For more information See [Github Swagger-UI](https://github.com/swagger-api/swagger-ui)
