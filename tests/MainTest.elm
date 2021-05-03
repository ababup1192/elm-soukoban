module MainTest exposing (..)

import Array
import Dict as Dict exposing (Dict)
import Expect
import Main exposing (..)
import Test exposing (..)


initTest : Test
initTest =
    describe "init test"
        [ describe "modelの初期化をしたとき"
            [ test "cellsの指定した場所以外はFloorになる" <|
                \_ ->
                    init
                        |> Tuple.first
                        |> .cells
                        |> Array.get 0
                        |> Expect.equal (Just Floor)
            ]
        ]


updateTest : Test
updateTest =
    describe "updateTest test"
        [ describe "KeyDownを受け取ったとき"
            [ test "移動先:FloorかつKeyDown:Leftの場合、Meは左方向に動く" <|
                \_ ->
                    update (KeyDown Left)
                        { sideCount = 2
                        , cells = Array.fromList [ Floor, Me ]
                        , baseCellPlaces = Dict.fromList []
                        }
                        |> Tuple.first
                        |> .cells
                        |> Expect.equal (Array.fromList [ Me, Floor ])
            , test "移動先:FloorかつKeyDown:Rightの場合、Meは右方向に動く" <|
                \_ ->
                    update (KeyDown Right)
                        { sideCount = 2
                        , cells = Array.fromList [ Me, Floor ]
                        , baseCellPlaces = Dict.fromList []
                        }
                        |> Tuple.first
                        |> .cells
                        |> Expect.equal (Array.fromList [ Floor, Me ])
            , test "移動先:FloorかつKeyDown:Upの場合、Meは上方向に動く" <|
                \_ ->
                    update (KeyDown Up)
                        { sideCount = 2
                        , cells = Array.fromList [ Floor, Floor, Me, Floor ]
                        , baseCellPlaces = Dict.fromList []
                        }
                        |> Tuple.first
                        |> .cells
                        |> Expect.equal (Array.fromList [ Me, Floor, Floor, Floor ])
            , test "移動先:FloorかつKeyDown:Downの場合、Meは下方向に動く" <|
                \_ ->
                    update (KeyDown Down)
                        { sideCount = 2
                        , cells = Array.fromList [ Me, Floor, Floor, Floor ]
                        , baseCellPlaces = Dict.fromList []
                        }
                        |> Tuple.first
                        |> .cells
                        |> Expect.equal (Array.fromList [ Floor, Floor, Me, Floor ])
            , test "移動先:ContainerでContainerも移動可能な場合、ContainerとMeは動く" <|
                \_ ->
                    update (KeyDown Left)
                        { sideCount = 3
                        , cells = Array.fromList [ Floor, Container, Me ]
                        , baseCellPlaces = Dict.fromList []
                        }
                        |> Tuple.first
                        |> .cells
                        |> Expect.equal (Array.fromList [ Container, Me, Floor ])
            , test "移動先:ContainerでContainerの移動先がWallの場合、ContainerとMeは動かない" <|
                \_ ->
                    update (KeyDown Left)
                        { sideCount = 3
                        , cells = Array.fromList [ Wall, Container, Me ]
                        , baseCellPlaces = Dict.fromList []
                        }
                        |> Tuple.first
                        |> .cells
                        |> Expect.equal (Array.fromList [ Wall, Container, Me ])
            , test "移動先:ContainerでContainerの移動先がContainerの場合、ContainerとMeは動かない" <|
                \_ ->
                    update (KeyDown Left)
                        { sideCount = 3
                        , cells = Array.fromList [ Container, Container, Me ]
                        , baseCellPlaces = Dict.fromList []
                        }
                        |> Tuple.first
                        |> .cells
                        |> Expect.equal (Array.fromList [ Container, Container, Me ])
            , test "移動先:ContainerでContainerの移動先にマスがない場合、ContainerとMeは動かない" <|
                \_ ->
                    update (KeyDown Left)
                        { sideCount = 2
                        , cells = Array.fromList [ Container, Me ]
                        , baseCellPlaces = Dict.fromList []
                        }
                        |> Tuple.first
                        |> .cells
                        |> Expect.equal (Array.fromList [ Container, Me ])
            , test "移動先:Wallの場合、Meは動かない" <|
                \_ ->
                    update (KeyDown Left)
                        { sideCount = 2
                        , cells = Array.fromList [ Wall, Me ]
                        , baseCellPlaces = Dict.fromList []
                        }
                        |> Tuple.first
                        |> .cells
                        |> Expect.equal (Array.fromList [ Wall, Me ])
            , test "移動先:マスがない場合、Meは動かない" <|
                \_ ->
                    update (KeyDown Left)
                        { sideCount = 2
                        , cells = Array.fromList [ Me, Floor ]
                        , baseCellPlaces = Dict.fromList []
                        }
                        |> Tuple.first
                        |> .cells
                        |> Expect.equal (Array.fromList [ Me, Floor ])
            , test "移動元にベースセル指定がなかった場合、移動元はFloorに戻る" <|
                \_ ->
                    update (KeyDown Left)
                        { sideCount = 2
                        , cells = Array.fromList [ Floor, Me ]
                        , baseCellPlaces = Dict.fromList []
                        }
                        |> Tuple.first
                        |> .cells
                        |> Array.get 1
                        |> Expect.equal (Just Floor)
            , test "移動元にベースセル指定があった場合、移動元はそのセルに戻る" <|
                \_ ->
                    update (KeyDown Left)
                        { sideCount = 2
                        , cells = Array.fromList [ Floor, Me ]
                        , baseCellPlaces = Dict.fromList [ ( 1, Cord ) ]
                        }
                        |> Tuple.first
                        |> .cells
                        |> Array.get 1
                        |> Expect.equal (Just Cord)
            ]
        ]
