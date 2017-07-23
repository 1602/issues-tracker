module Request.Helpers exposing (authHeader, withAuthorization, apiUrl)

import HttpBuilder exposing (RequestBuilder, withHeader)
import Base64


-- import Task

import Http exposing (Error, Response)


apiUrl : String -> String
apiUrl path =
    "https://api.github.com" ++ path


authHeader : String -> Http.Header
authHeader secretKey =
    Http.header "Authorization" <|
        "Basic "
            ++ (secretKey ++ ":" |> Base64.encode |> Result.withDefault "")


withAuthorization : String -> RequestBuilder a -> RequestBuilder a
withAuthorization token builder =
    builder
        |> withHeader "authorization" ("Basic " ++ (token ++ ":" |> Base64.encode |> Result.withDefault ""))
