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
    describe "view"
        [ test "resourcePage has dark background top bar" <|
            \_ ->
                Main.page
                    |> find Main.TopBar
                    |> Maybe.map Main.styles
                    |> Maybe.andThen .backgroundColor
                    |> Expect.equal (Just Main.AlmostBlack)
        , test "top bar contains search input" <|
            \_ ->
                Main.page
                    |> find Main.TopBar
                    |> Maybe.andThen (find Main.SearchInput)
                    |> (/=) Nothing
                    |> Expect.true "could not find search input"
        , test "top bar has transparent background" <|
            \_ ->
                Main.page
                    |> find Main.TopBar
                    |> Maybe.andThen (find Main.SearchInput)
                    |> Maybe.map Main.styles
                    |> Maybe.andThen .backgroundColor
                    |> Expect.equal (Just Main.Transparent)
        , test "top bar has monospace font" <|
            \_ ->
                Main.page
                    |> find Main.TopBar
                    |> Maybe.andThen (find Main.SearchInput)
                    |> Maybe.map Main.styles
                    |> Maybe.andThen .fontFamily
                    |> Expect.equal (Just Main.Inconsolata)
        , test "top bar has light grey border" <|
            \_ ->
                Main.page
                    |> find Main.TopBar
                    |> Maybe.andThen (find Main.SearchInput)
                    |> Maybe.map Main.styles
                    |> Maybe.andThen .border
                    |> Expect.equal (Just Main.LighterGrey)
        ]
