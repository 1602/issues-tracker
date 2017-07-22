module Request.Helpers exposing (authHeader, cachingFetch, withAuthorization)

import HttpBuilder exposing (RequestBuilder, withHeader)

import Base64
-- import Task
import Http exposing (Error, Response)
import Messages exposing (..)
import Models exposing (..)
import Dict
import Models exposing (Model)

authHeader : String -> Http.Header
authHeader secretKey =
    Http.header "Authorization" <|
        "Basic "
            ++ (secretKey ++ ":" |> Base64.encode |> Result.withDefault "")



cachingFetch : String -> Dict.Dict String (String, String) -> (String -> Msg) -> Cmd Msg
cachingFetch url etags oncomplete =
    let
        (etag, cachedBody) =
            case Dict.get url etags of
                Just (etag, body) ->
                    (etag, body)

                Nothing ->
                    ("", "")

        extractEtag res =
            res.headers
                |> Dict.toList
                |> List.filterMap (\(key, val) ->
                    if "etag" == (String.toLower key) then
                        Just val
                    else
                        Nothing
                    )
                |> List.head
    in
        Http.request
            { method = "GET"
            , headers =
                if etag /= "" then
                    [ Http.header "If-None-Match" etag ]
                else
                    [ Http.header "If-Modified-Since" "0" ]
            , url = url
            , expect = Http.expectStringResponse (\res ->
                if res.status.code == 304 then
                    Ok <| CachedData res.url etag cachedBody
                else
                    case extractEtag res of
                        Just etag ->
                            Ok <| CachedData res.url etag res.body

                        Nothing ->
                            Ok <| NotCached res.body
                )
            , body = Http.emptyBody
            , timeout = Nothing
            , withCredentials = False
            }
            |> Http.send (FetchComplete oncomplete)


withAuthorization : String -> RequestBuilder a -> RequestBuilder a
withAuthorization token builder =
    builder
        |> withHeader "authorization" ("Basic " ++ (token ++ ":" |> Base64.encode |> Result.withDefault ""))

