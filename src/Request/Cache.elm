module Request.Cache exposing (withCache, Etags, RemoteData(..), updateCache, retrieveError, retrieveData)

import Http exposing (Error(..))
import HttpBuilder exposing (withHeader, RequestBuilder)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, decodeString)


type alias Etags =
    Dict String String


type RemoteData a
    = CanBeCached String String a
    | CanNotBeCached a


withCache : Etags -> Decoder a -> RequestBuilder () -> RequestBuilder (RemoteData a)
withCache etags decoder rb =
    let
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

        ( cacheHeaderName, cacheHeaderValue ) =
            case Dict.get rb.url etags of
                Just etag ->
                    ( "If-None-Match", etag )
                Nothing ->
                    ( "If-Modified-Since", "0" )

        decode str fn =
            case decodeString decoder str of
                Ok x -> Ok (fn x)
                Err y -> Err y

        expect =
            Http.expectStringResponse
                (\res ->
                    case extractEtag res of
                        Just etag ->
                            decode res.body (CanBeCached res.url etag)

                        Nothing ->
                            decode res.body CanNotBeCached
                )
    in
        rb
            |> HttpBuilder.withExpect expect
            |> withHeader cacheHeaderName cacheHeaderValue


updateCache : Result Error (RemoteData a) -> Etags -> Etags
updateCache result cache = 
    case result of
        Ok (CanBeCached url etag _) ->
            Dict.insert url etag cache

        _ ->
            cache


retrieveError : Result Error a -> Maybe String
retrieveError result =
    case result of
        Err (BadStatus res) ->
            if res.status.code == 304 then
                Nothing
            else
                Just <| toString e

        Err e ->
            Just <| toString e

        _ ->
            Nothing


retrieveData : Result x (RemoteData a) -> a -> a
retrieveData result prevData =
    case result of
        Ok (CanBeCached _ _ data) ->
            data
        _ ->
            prevData
