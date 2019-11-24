module Main exposing (main)

import Browser
import Html as H exposing (Html)
import Html.Attributes as A
import OrderableList


type alias Card =
    { title : String
    , message : String
    , author : String
    }


type alias Model =
    { list : OrderableList.Model Card
    , statusText : String
    }


type Msg
    = UpdateList OrderableList.Msg


config : OrderableList.Config
config =
    { elementHeight = 40 }


cards : List Card
cards =
    List.range 1 6
        |> List.map
            (\i ->
                let
                    s =
                        String.fromInt i
                in
                    { title = "note " ++ s
                    , message = "message " ++ s
                    , author = "author " ++ s
                    }
            )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init () =
    let
        ( listModel, listCmd ) =
            OrderableList.init config cards
    in
        ( { list = listModel
          , statusText = ""
          }
        , Cmd.map UpdateList listCmd
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateList listMsg ->
            case OrderableList.update listMsg model.list of
                OrderableList.UpdateState ( listModel, listCmd ) ->
                    ( { model | list = listModel }, Cmd.map UpdateList listCmd )

                OrderableList.ElementDropped element index ->
                    let
                        statusText =
                            "Dropped card titled '" ++ element.title ++ "' at index " ++ String.fromInt index
                    in
                        ( { model | statusText = statusText }, Cmd.none )


view : Model -> Html Msg
view model =
    H.div []
        [ H.div [ A.class "card-list" ]
            [ OrderableList.view UpdateList renderCard model.list ]
        , H.text model.statusText
        ]


renderCard : Card -> Html Msg
renderCard card =
    H.div [ A.class "card" ]
        [ H.text (card.title ++ " by " ++ card.author)
        , H.br [] []
        , H.text card.message
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map UpdateList (OrderableList.subscriptions model.list)
