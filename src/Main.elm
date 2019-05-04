module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, src, style, type_, value)
import Html.Events exposing (on, onClick, onInput, onSubmit, targetValue)
import Http
import Image exposing (Format(..), Image, filterImageFormat, imageListDecoder)



-- Model


type alias Model =
    { searchTerms : String
    , images : List Image
    , error : String
    , format : Format
    }


type Msg
    = InputChanged String
    | FormSubmitted
    | ResponseReceived (Result Http.Error (List Image))
    | DeleteErrorMessage
    | FormatChanged String


init : () -> ( Model, Cmd Msg )
init _ =
    ( { searchTerms = ""
      , images = []
      , error = ""
      , format = Any
      }
    , Cmd.none
    )



-- View


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "section" ]
            [ h1 [ class "title" ] [ text "elm image search" ]
            , viewForm
            , viewResults model
            ]
        ]


viewForm : Html Msg
viewForm =
    form [ onSubmit FormSubmitted, style "margin-bottom" "20px" ]
        [ div [ class "field" ]
            [ div [ class "control" ]
                [ input
                    [ type_ "text"
                    , class "medium input"
                    , onInput InputChanged
                    ]
                    []
                ]
            ]
        , div [ class "field" ]
            [ div [ class "control" ]
                [ div [ class "select" ]
                    [ select
                        [ onInput FormatChanged ]
                        [ option [ value "any" ] [ text "Tous" ]
                        , option [ value "landscape" ] [ text "Paysage" ]
                        , option [ value "portrait" ] [ text "Portrait" ]
                        ]
                    ]
                ]
            ]
        ]


hasError : Model -> Bool
hasError model =
    not <| String.isEmpty model.error


viewResults : Model -> Html Msg
viewResults model =
    if hasError model then
        viewError model

    else
        viewList model


viewList : Model -> Html Msg
viewList model =
    div [ class "columns is-multiline" ] (List.map viewThumbnail <| filterImageFormat model.format model.images)


viewThumbnail : Image -> Html Msg
viewThumbnail image =
    div [ class "column is-one-quarter" ]
        [ img [ src image.thumbnailUrl ] []
        ]


viewError : Model -> Html Msg
viewError model =
    article [ class "message is-danger" ]
        [ div [ class "message-header" ]
            [ p [] [ text "Oups..." ]
            , button [ class "delete", onClick DeleteErrorMessage ] []
            ]
        , div [ class "message-body" ]
            [ text model.error ]
        ]



--Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputChanged value ->
            ( { model | searchTerms = value }, Cmd.none )

        FormSubmitted ->
            let
                httpCommand =
                    Http.get
                        { url =
                            "http://unsplash.noprod-b.kmt.orange.com"
                                ++ "/search/photos?query="
                                ++ model.searchTerms
                        , expect = Http.expectJson ResponseReceived imageListDecoder -- 2
                        }
            in
            ( model, httpCommand )

        ResponseReceived (Err _) ->
            ( { model | error = "Ouch, something went wrong calling API" }, Cmd.none )

        ResponseReceived (Ok imageList) ->
            ( { model | images = imageList }, Cmd.none )

        DeleteErrorMessage ->
            ( { model | error = "" }, Cmd.none )

        FormatChanged value ->
            case value of
                "landscape" ->
                    ( { model | format = Landscape }, Cmd.none )

                "portrait" ->
                    ( { model | format = Portrait }, Cmd.none )

                _ ->
                    ( { model | format = Any }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
