# servant-gp

This is a demo project for [servant](https://haskell-servant.github.io/) and [generic-persistence](https://github.com/thma/generic-persistence).

It shows how to use servant to build a REST API for a simple persistence model.
The Data is stored in a Sqlite database using generic-persistence as persistence layer.

The model is defined in `src/Models.hs` and the API is defined in `src/UserApi.hs`.

The API is implemented in `src/UserServer.hs` (with the GP default API) and in `src/UserServerSafe.hs` (using an API with total functions, where exceptions are returned as `Left PersistenceException`).

The `app/Main.hs` module contains the main function to start the server.

`src/SwaggerEntityService` contains a servant-swagger module to generate a swagger API description.