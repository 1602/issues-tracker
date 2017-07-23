module Request.Helpers exposing (authHeader, cachingFetch, withAuthorization, apiUrl)

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


cachingFetch : String -> String -> Dict.Dict String ( String, String ) -> (String -> Msg) -> Cmd Msg
cachingFetch url accessToken etags oncomplete =
    let
        ( etag, cachedBody ) =
            case Dict.get url etags of
                Just ( etag, body ) ->
                    ( etag, body )

                Nothing ->
                    ( "", "" )

        extractEtag res =
            res.headers
                |> Dict.toList
                |> List.filterMap
                    (\( key, val ) ->
                        if "etag" == (String.toLower key) then
                            Just val
                        else
                            Nothing
                    )
                |> List.head

        expect =
            Http.expectStringResponse
                (\res ->
                    if res.status.code == 304 then
                        Ok <| CachedData res.url etag cachedBody
                    else
                        case extractEtag res of
                            Just etag ->
                                Ok <| CachedData res.url etag res.body

                            Nothing ->
                                Ok <| NotCached res.body
                )

        ( cacheHeaderName, cacheHeaderValue ) =
            if etag /= "" then
                ( "If-None-Match", etag )
            else
                ( "If-Modified-Since", "0" )
    in
        url
            |> HttpBuilder.get
            |> HttpBuilder.withExpect expect
            |> withAuthorization accessToken
            |> withHeader cacheHeaderName cacheHeaderValue
            |> HttpBuilder.toRequest
            |> Http.send (FetchComplete oncomplete)


withAuthorization : String -> RequestBuilder a -> RequestBuilder a
withAuthorization token builder =
    builder
        |> withHeader "authorization" ("Basic " ++ (token ++ ":" |> Base64.encode |> Result.withDefault ""))
