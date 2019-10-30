module OrderableList exposing (Config, Model, Msg, init, subscriptions, update, view)

import Dict exposing (Dict)
import Draggable
import Draggable.Events as DraggableEvents
import Html as H exposing (Html)
import Html.Attributes as A
import List.Extra as ListExtra
import Time


type Model a
    = Model
        { elementOrder : List Int
        , elements : Dict Int a
        , drag : Draggable.State Int
        , currentlyDragging : Maybe Dragging
        , release : ReleaseAnimation
        , config : Config
        }


type Msg
    = Draggable (Draggable.Msg Int)
    | OnDragBy Draggable.Delta
    | OnDragStart Int
    | OnDragEnd
    | StepReleaseAnimation


type alias Config =
    { elementHeight : Int
    }


init : Config -> List a -> ( Model a, Cmd Msg )
init config apps =
    let
        ( _, appsDict ) =
            List.foldl
                (\app ( id, acc ) ->
                    ( id + 1, Dict.insert id app acc )
                )
                ( 0, Dict.empty )
                apps
    in
        ( Model
            { elementOrder = List.range 0 (List.length apps - 1)
            , elements = appsDict
            , drag = Draggable.init
            , currentlyDragging = Nothing
            , config = config
            , release = ReleaseNone
            }
        , Cmd.none
        )


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg (Model model) =
    case msg of
        Draggable draggableMsg ->
            let
                ( newModel, cmd ) =
                    Draggable.update dragConfig draggableMsg model
            in
                ( Model newModel, cmd )

        OnDragBy ( _, dy ) ->
            case model.currentlyDragging of
                Nothing ->
                    ( Model model, Cmd.none )

                Just dragging ->
                    ( Model
                        { model
                            | currentlyDragging =
                                Just { dragging | offset = dragging.offset + dy }
                        }
                    , Cmd.none
                    )

        OnDragStart id ->
            let
                maybeIndex =
                    ListExtra.elemIndex id model.elementOrder
            in
                case maybeIndex of
                    Nothing ->
                        ( Model model, Cmd.none )

                    Just index ->
                        ( Model
                            { model
                                | currentlyDragging =
                                    Just
                                        { subjectId = id
                                        , subjectOrderIndex = index
                                        , offset = 0.0
                                        }
                            }
                        , Cmd.none
                        )

        OnDragEnd ->
            case model.currentlyDragging of
                Just dragging ->
                    let
                        newIndex =
                            clamp
                                0
                                (List.length model.elementOrder - 1)
                                (calculateNewIndex dragging model.config)

                        newOrder =
                            if newIndex == dragging.subjectOrderIndex then
                                model.elementOrder

                            else
                                ListExtra.indexedFoldr
                                    (\idx element orderAcc ->
                                        if idx == newIndex then
                                            if newIndex > dragging.subjectOrderIndex then
                                                element :: dragging.subjectId :: orderAcc

                                            else if newIndex < dragging.subjectOrderIndex then
                                                dragging.subjectId :: element :: orderAcc

                                            else
                                                element :: orderAcc

                                        else if idx == dragging.subjectOrderIndex then
                                            orderAcc

                                        else
                                            element :: orderAcc
                                    )
                                    []
                                    model.elementOrder
                    in
                        ( Model
                            { model
                                | currentlyDragging = Nothing
                                , elementOrder = newOrder
                                , release =
                                    ReleaseJustReleased
                                        dragging.subjectId
                                        (dragging.offset - toFloat ((newIndex - dragging.subjectOrderIndex) * fullHeight model.config))
                            }
                        , Cmd.none
                        )

                Nothing ->
                    ( Model model, Cmd.none )

        StepReleaseAnimation ->
            let
                newAnimationState =
                    case model.release of
                        ReleaseNone ->
                            ReleaseNone

                        ReleaseAnimating _ ->
                            ReleaseNone

                        ReleaseJustReleased id _ ->
                            ReleaseAnimating id
            in
                ( Model { model | release = newAnimationState }, Cmd.none )


view : (a -> Html Msg) -> Model a -> Html Msg
view viewElement (Model model) =
    let
        getOffset index id =
            case model.currentlyDragging of
                Just dragging ->
                    if id == dragging.subjectId then
                        Just dragging.offset

                    else
                        let
                            oldIndex =
                                dragging.subjectOrderIndex

                            newIndex =
                                oldIndex + orderOffset model.config dragging.offset
                        in
                            Just <|
                                if index > oldIndex && newIndex >= index then
                                    -(toFloat <| fullHeight model.config)

                                else if index < oldIndex && newIndex <= index then
                                    toFloat <| fullHeight model.config

                                else
                                    0.0

                Nothing ->
                    case model.release of
                        ReleaseNone ->
                            Nothing

                        ReleaseAnimating releasedId ->
                            if releasedId == id then
                                Just 0.0

                            else
                                Nothing

                        ReleaseJustReleased releasedId releasedOffset ->
                            if releasedId == id then
                                Just releasedOffset

                            else
                                Nothing

        animatingRelease id =
            case model.release of
                ReleaseNone ->
                    False

                ReleaseAnimating releasedId ->
                    releasedId == id

                ReleaseJustReleased _ _ ->
                    False

        justReleased id =
            case model.release of
                ReleaseNone ->
                    False

                ReleaseAnimating _ ->
                    False

                ReleaseJustReleased releasedId _ ->
                    releasedId == id

        isDragged id =
            case model.currentlyDragging of
                Nothing ->
                    False

                Just dragging ->
                    id == dragging.subjectId

        elements =
            ListExtra.indexedFoldr
                (\index id html ->
                    case Dict.get id model.elements of
                        Nothing ->
                            html

                        Just element ->
                            renderElement
                                model.config.elementHeight
                                (isDragged id)
                                (animatingRelease id)
                                (justReleased id)
                                (getOffset index id)
                                id
                                (viewElement element)
                                :: html
                )
                []
                model.elementOrder
    in
        H.div [] elements


subscriptions : Model a -> Sub Msg
subscriptions (Model model) =
    Sub.batch
        [ Draggable.subscriptions Draggable model.drag
        , case model.release of
            ReleaseNone ->
                Sub.none

            ReleaseAnimating _ ->
                Time.every 200 <| always StepReleaseAnimation

            ReleaseJustReleased _ _ ->
                Time.every 20 <| always StepReleaseAnimation
        ]



{- INTERNAL -}


type alias Dragging =
    { subjectId : Int
    , subjectOrderIndex : Int
    , offset : Float
    }


type ReleaseAnimation
    = ReleaseNone
    | ReleaseAnimating Int
    | ReleaseJustReleased Int Float


border : Int
border =
    1


margin : Int
margin =
    5


fullHeight : Config -> Int
fullHeight config =
    border * 2 + margin + config.elementHeight


orderOffset : Config -> Float -> Int
orderOffset config offset =
    round (offset / toFloat (fullHeight config))


calculateNewIndex : Dragging -> Config -> Int
calculateNewIndex dragging config =
    dragging.subjectOrderIndex + orderOffset config dragging.offset


dragConfig : Draggable.Config Int Msg
dragConfig =
    Draggable.customConfig
        [ DraggableEvents.onDragBy OnDragBy
        , DraggableEvents.onDragStart OnDragStart
        , DraggableEvents.onDragEnd OnDragEnd
        ]


renderElement : Int -> Bool -> Bool -> Bool -> Maybe Float -> Int -> Html Msg -> Html Msg
renderElement elementHeight dragged animatingRelease justReleased maybeOffset id element =
    H.div
        [ Draggable.mouseTrigger id Draggable
        , case maybeOffset of
            Just offset ->
                A.style "transform" ("translate(0, " ++ String.fromFloat offset ++ "px)")

            Nothing ->
                A.attribute "" ""
        , A.style "border" (String.fromInt border ++ "px solid black")
        , A.style "margin" (String.fromInt margin ++ "px")
        , A.style "height" (String.fromInt elementHeight ++ "px")
        , A.style "transition"
            (if justReleased || not animatingRelease && (dragged || maybeOffset == Nothing) then
                "none"

             else
                "transform 200ms ease-in-out"
            )
        ]
        [ element ]