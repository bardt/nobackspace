module App exposing (..)

import Dom exposing (Id)
import Html exposing (Html, div, img, input, p, span, text, textarea)
import Html.Attributes exposing (autofocus, class, id, value)
import Html.Events exposing (keyCode, on, onInput)
import Json.Decode as Json
import Mouse
import Task


type alias Model =
    { text : String
    }


init : ( Model, Cmd Msg )
init =
    ( { text = "There is no backspace"
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | Input String
    | NewLine
    | ShowCursor


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input text ->
            ( { model
                | text = model.text ++ text
              }
            , Cmd.none
            )

        NewLine ->
            ( { model
                | text = model.text ++ "\n"
              }
            , Cmd.none
            )

        ShowCursor ->
            ( model, Dom.focus (inputId) |> Task.attempt (\_ -> NoOp) )

        NoOp ->
            ( model, Cmd.none )


inputId : String
inputId =
    "cursor"


view : Model -> Html Msg
view model =
    div []
        [ p []
            [ text model.text
            , cursor model
            ]
        ]


cursor : Model -> Html Msg
cursor _ =
    input
        [ class "cursor"
        , autofocus True
        , onInput Input
        , value ""
        , id inputId
        , on "keyup" (Json.map toNewLine keyCode)
        ]
        []


toNewLine : Int -> Msg
toNewLine key =
    if key == 13 then
        NewLine
    else
        NoOp


subscriptions : Model -> Sub Msg
subscriptions model =
    Mouse.clicks (\_ -> ShowCursor)
