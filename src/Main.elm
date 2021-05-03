module Main exposing (..)

import Array exposing (Array, indexedMap)
import Browser
import Browser.Events exposing (onKeyDown)
import Dict as Dict exposing (Dict)
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (src, style)
import Json.Decode as JD


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Board =
    Array Cell


type GameStatus
    = Play
    | Clear


type alias Model =
    { gameStatus : GameStatus
    , sideCount : Int
    , cells : Board
    , baseCellPlaces : Dict Int Cell
    }


init : ( Model, Cmd Msg )
init =
    let
        sideCount =
            5

        myPoint =
            8

        initialCellPlaces =
            [ ( myPoint, Me ), ( 12, Container ), ( 13, Container ) ]

        baseCellPlaces =
            [ ( 3, Wall )
            , ( 4, Wall )
            , ( 5, Cord )
            , ( 10, Cord )
            ]

        cells =
            Array.initialize
                (sideCount * sideCount)
                (\n ->
                    Maybe.withDefault Floor <| Dict.get n <| Dict.fromList <| baseCellPlaces ++ initialCellPlaces
                )
    in
    ( { gameStatus = Play
      , sideCount = sideCount
      , cells = cells
      , baseCellPlaces = Dict.fromList baseCellPlaces
      }
    , Cmd.none
    )


type Cell
    = Floor
    | Wall
    | Container
    | Cord
    | Me



-- UPDATE


type Msg
    = KeyDown Direction


type Direction
    = Left
    | Up
    | Right
    | Down


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        KeyDown direction ->
            let
                current =
                    case find (\cell -> Tuple.second cell == Me) (List.indexedMap Tuple.pair (Array.toList model.cells)) of
                        Just ( i, _ ) ->
                            i

                        Nothing ->
                            0

                nextMyPoint =
                    move { direction = direction, sideCount = model.sideCount, current = current } model.cells Me

                nextCellMaybe =
                    Array.get nextMyPoint model.cells

                nextContainerPoint =
                    move { direction = direction, sideCount = model.sideCount, current = nextMyPoint } model.cells Container

                formerCell =
                    Maybe.withDefault Floor <| Dict.get current model.baseCellPlaces

                setCells =
                    setCell { cell = Me, point = nextMyPoint }
                        << setCell { cell = formerCell, point = current }

                cells =
                    if nextCellMaybe == Just Container && nextContainerPoint == nextMyPoint then
                        model.cells

                    else if nextCellMaybe == Just Container then
                        (setCells
                            << setCell
                                { cell = Container
                                , point = nextContainerPoint
                                }
                        )
                            model.cells

                    else
                        setCells model.cells
            in
            ( { model
                | gameStatus = judgeGameStatus model.baseCellPlaces cells
                , cells = cells
              }
            , Cmd.none
            )


type alias MoveArg =
    { direction : Direction, sideCount : Int, current : Int }


move : MoveArg -> Board -> Cell -> Int
move { direction, sideCount, current } board cell =
    let
        destination : Int
        destination =
            moveHelper { direction = direction, sideCount = sideCount, current = current }
    in
    case Array.get destination board of
        Just Wall ->
            current

        Just Container ->
            case cell of
                Container ->
                    current

                _ ->
                    destination

        _ ->
            destination


moveHelper : MoveArg -> Int
moveHelper { direction, sideCount, current } =
    current
        + (if
            (modBy sideCount current == 0 && direction == Left)
                || (current < sideCount && direction == Up)
                || (modBy sideCount current == sideCount - 1 && direction == Right)
                || ((current + sideCount) >= (sideCount * sideCount) && direction == Down)
           then
            0

           else
            case direction of
                Left ->
                    -1

                Up ->
                    -sideCount

                Right ->
                    1

                Down ->
                    sideCount
          )


setCell : { cell : Cell, point : Int } -> Board -> Board
setCell { cell, point } =
    Array.set point cell


judgeGameStatus : Dict Int Cell -> Board -> GameStatus
judgeGameStatus baseCellPlaces board =
    let
        cordCellIndexs =
            List.map (\( f, _ ) -> f) <|
                Dict.toList <|
                    Dict.filter (\d cell -> cell == Cord) baseCellPlaces

        cordWithObjects =
            List.map (\i -> Array.get i board) cordCellIndexs
    in
    if
        List.isEmpty <|
            List.filter (\cellMaybe -> cellMaybe /= Just Container) cordWithObjects
    then
        Clear

    else
        Play



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions model =
    onKeyDown <| JD.map (\d -> KeyDown d) keyDecoder


keyDecoder : JD.Decoder Direction
keyDecoder =
    JD.andThen toDirection (JD.field "key" JD.string)


toDirection : String -> JD.Decoder Direction
toDirection string =
    case string of
        "ArrowLeft" ->
            JD.succeed Left

        "ArrowUp" ->
            JD.succeed Up

        "ArrowRight" ->
            JD.succeed Right

        "ArrowDown" ->
            JD.succeed Down

        _ ->
            JD.fail "can use direction key only"



-- VIEW


view : Model -> Html Msg
view model =
    let
        attrs =
            [ style "display" "flex"
            , style "justify-content" "center"
            ]
    in
    div attrs
        [ viewBoard model, viewGameStatus model ]


viewBoard : Model -> Html msg
viewBoard model =
    let
        sideLength =
            vmin boardLength

        cellSideLength =
            boardLength / toFloat model.sideCount

        attrs =
            [ style "width" sideLength
            , style "height" sideLength
            ]
    in
    div attrs
        (List.map (\cell -> viewCell cellSideLength cell) (Array.toList model.cells))


viewCell : Float -> Cell -> Html msg
viewCell sideLength cell =
    let
        styles =
            [ style "width" <| vmin sideLength
            , style "height" <| vmin sideLength
            , style "margin" "0"
            , style "display" "block"
            , style "float" "left"
            , style "border" "0.2vmin solid gray"
            , style "box-sizing" "border-box"
            ]
    in
    img (src (floor cell) :: styles) []


viewGameStatus : Model -> Html msg
viewGameStatus model =
    let
        status =
            case model.gameStatus of
                Clear ->
                    "ゲームクリア"

                _ ->
                    ""
    in
    div [] [ text status ]


floor : Cell -> String
floor cell =
    case cell of
        Floor ->
            "https://thumb.photo-ac.com/f3/f36043bd98e3ba19a7b7f4fca230adae_t.jpeg"

        Wall ->
            "https://publicdomainq.net/images/201709/24s/publicdomainq-0013684agf.jpg"

        Container ->
            "https://japaclip.com/files/rock.png"

        Cord ->
            "https://www.photolibrary.jp/mhd5/img229/450-20111117154918174234.jpg"

        Me ->
            "https://cdn-ak.f.st-hatena.com/images/fotolife/o/owata047/20170415/20170415213544.jpg"


boardLength : Float
boardLength =
    80.0


vmin : Float -> String
vmin n =
    String.append (String.fromFloat n) "vmin"


find : (a -> Bool) -> List a -> Maybe a
find f list =
    List.head <| List.filter f list
