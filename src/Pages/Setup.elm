module Pages.Setup exposing (Msg, Model, init, view, update)

import Html exposing (Html, text, div)
import Html.Events exposing (onInput, onClick)
import Html.Attributes as Attrs


type Msg
    = EditAccessToken String
    | SaveAccessToken


type alias Model =
    { token : String
    }


init : Model
init =
    Model ""


view : Html Msg
view =
    div []
        [ text "cheers. visit "
        , Html.a [ Attrs.href "https://github.com/settings/tokens" ] [ text "https://github.com/settings/tokens" ]
        , text " (we need 'repo' access granted to see all private repositories)"
        , Html.br [] []
        , text "and fill this input "
        , Html.input [ onInput EditAccessToken ] []
        , Html.button [ onClick SaveAccessToken ] [ text "then press this button" ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditAccessToken token ->
            { model | token = token } ! []

        SaveAccessToken ->
            model ! []
