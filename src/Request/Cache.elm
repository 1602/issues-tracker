module Request.Cache exposing (cachingFetch)

import Http
import HttpBuilder exposing (withHeader)
import Messages exposing (..)
import Dict exposing (Dict)
import Models exposing (CachedData(..))
import Request.Helpers exposing (withAuthorization)

cachingFetch : String -> String -> Dict String ( String, String ) -> (String -> Msg) -> Cmd Msg
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

