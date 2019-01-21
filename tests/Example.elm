module Example exposing (suite)

import Expect
import Main
import Test exposing (..)


find : Main.Id -> Main.Node -> Maybe Main.Node
find id ((Main.Node iden _ _ _ _) as node) =
    if iden == Main.Id id then
        Just node

    else
        node
            |> Main.children
            |> List.filterMap (find id)
            |> List.head


suite : Test
suite =
    describe "view" <|
        let
            topBar : () -> Maybe Main.Node
            topBar _ =
                Main.page |> find Main.TopBar
        in
        [ test "resourcePage has dark background top bar" <|
            topBar
                >> Maybe.map Main.styles
                >> Maybe.andThen .backgroundColor
                >> Expect.equal (Just Main.AlmostBlack)
        , describe "search input" <|
            let
                searchInput : () -> Maybe Main.Node
                searchInput =
                    topBar >> Maybe.andThen (find Main.SearchInput)
            in
            [ test "top bar contains search input" <|
                searchInput
                    >> (/=) Nothing
                    >> Expect.true "could not find search input"
            , test "search bar has transparent background" <|
                searchInput
                    >> Maybe.map Main.styles
                    >> Maybe.andThen .backgroundColor
                    >> Expect.equal (Just Main.Transparent)
            , test "search bar has monospace font" <|
                searchInput
                    >> Maybe.map Main.styles
                    >> Maybe.andThen .fontFamily
                    >> Expect.equal (Just Main.Inconsolata)
            , test "search bar has light grey border" <|
                searchInput
                    >> Maybe.map Main.styles
                    >> Maybe.andThen .border
                    >> Expect.equal (Just Main.LighterGrey)
            ]
        ]
