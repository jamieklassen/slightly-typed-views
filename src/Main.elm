module Main exposing
    ( BackgroundImage
    , Class
    , Color(..)
    , FontFamily(..)
    , Icon
    , Id(..)
    , Identifier(..)
    , Justification(..)
    , Layout
    , LayoutType(..)
    , Node(..)
    , NodeType(..)
    , Styles
    , cancelButton
    , children
    , classToString
    , concourseLogo
    , defaultLayout
    , defaultStyles
    , getId
    , idAttr
    , idToString
    , layoutAttr
    , main
    , page
    , searchContainer
    , searchInput
    , styleAttr
    , styles
    , toHash
    , topBar
    , userMenu
    , view
    , viewFunc
    )

import Html exposing (Html)
import Html.Attributes exposing (class, id, placeholder, style)



-- DATA ("PURE CORE")


type Color
    = AlmostBlack
    | MiddleGrey
    | LighterGrey
    | Transparent
    | White


type alias Styles =
    { backgroundColor : Maybe Color
    , icon : Maybe Icon
    , fontFamily : Maybe FontFamily
    , fontColor : Maybe Color
    , fontSize : Maybe String
    , backgroundImage : Maybe BackgroundImage
    , border : Maybe Color
    , hideOutline : Bool
    }


type FontFamily
    = Inconsolata


type alias Icon =
    { image : String, size : String }


type alias BackgroundImage =
    { image : String, position : String }


defaultStyles : Styles
defaultStyles =
    { backgroundColor = Nothing
    , icon = Nothing
    , fontFamily = Nothing
    , fontColor = Nothing
    , fontSize = Nothing
    , backgroundImage = Nothing
    , border = Nothing
    , hideOutline = False
    }


type alias Layout =
    { height : Maybe String
    , width : Maybe String
    , layoutType : LayoutType
    , margin : Maybe String
    , padding : Maybe String
    , anchorDescendants : Bool
    }


type Justification
    = NoJustification
    | Spread


type LayoutType
    = Column
    | Row Justification


defaultLayout : Layout
defaultLayout =
    { height = Nothing
    , width = Nothing
    , layoutType = Column
    , margin = Nothing
    , padding = Nothing
    , anchorDescendants = False
    }


type NodeType
    = Default
    | Button
    | TextInput String


type Node
    = Node Identifier NodeType Styles Layout (List Node)


type Identifier
    = Id Id
    | Class Class
    | Unidentified


type Id
    = Page
    | TopBar
    | ConcourseLogo
    | SearchInput


type alias Class =
    ()


children : Node -> List Node
children (Node _ _ _ _ children) =
    children


styles : Node -> Styles
styles (Node _ _ styles _ _) =
    styles


getId : Node -> Identifier
getId (Node iden _ _ _ _) =
    iden


page : Node
page =
    Node (Id Page)
        Default
        { defaultStyles | backgroundColor = Just MiddleGrey }
        { defaultLayout | height = Just "100%" }
        [ topBar ]


topBar : Node
topBar =
    Node (Id TopBar)
        Default
        { defaultStyles | backgroundColor = Just AlmostBlack }
        { defaultLayout
            | height = Just "54px"
            , layoutType = Row Spread
        }
        [ concourseLogo
        , searchContainer
        , userMenu
        ]


concourseLogo : Node
concourseLogo =
    let
        url =
            "https://ci.concourse-ci.org/public/images/concourse-logo-white.svg"
    in
    Node (Id ConcourseLogo)
        Default
        { defaultStyles
            | icon = Just { image = url, size = "42px" }
        }
        { defaultLayout | height = Just "54px", width = Just "54px" }
        []


searchContainer : Node
searchContainer =
    Node Unidentified
        Default
        defaultStyles
        { defaultLayout
            | margin = Just "12px"
            , anchorDescendants = True
        }
        [ searchInput
        , cancelButton
        ]


searchInput : Node
searchInput =
    let
        url =
            "https://ci.concourse-ci.org/public/images/ic-search-white-24px.svg"
    in
    Node (Id SearchInput)
        (TextInput "search")
        { defaultStyles
            | backgroundColor = Just Transparent
            , fontFamily = Just Inconsolata
            , fontColor = Just White
            , fontSize = Just "1.15em"
            , border = Just LighterGrey
            , hideOutline = True
            , backgroundImage =
                Just
                    { image = url
                    , position = "12px 8px"
                    }
        }
        { defaultLayout
            | width = Just "220px"
            , height = Just "30px"
            , padding = Just "0 42px"
        }
        []


cancelButton : Node
cancelButton =
    Node Unidentified Default defaultStyles defaultLayout []


userMenu : Node
userMenu =
    Node Unidentified Default defaultStyles defaultLayout []



-- ADAPTER ("IMPERATIVE SHELL")


toHash : Color -> String
toHash color =
    case color of
        White ->
            "#fff"

        AlmostBlack ->
            "#1e1d1d"

        MiddleGrey ->
            "#3d3c3c"

        LighterGrey ->
            "#504b4b"

        Transparent ->
            "transparent"


styleAttr : Styles -> Html.Attribute msg
styleAttr styles =
    style <|
        (case styles.backgroundColor of
            Just color ->
                [ ( "background-color", toHash color ) ]

            Nothing ->
                []
        )
            ++ (case styles.icon of
                    Just { image, size } ->
                        [ ( "background-image", "url(" ++ image ++ ")" )
                        , ( "background-position", "50% 50%" )
                        , ( "background-repeat", "no-repeat" )
                        , ( "background-size", size ++ " " ++ size )
                        ]

                    Nothing ->
                        []
               )
            ++ (case styles.fontFamily of
                    Just Inconsolata ->
                        [ ( "font-family", "Inconsolata, monospace" ) ]

                    Nothing ->
                        []
               )
            ++ (case styles.fontColor of
                    Just color ->
                        [ ( "color", toHash color ) ]

                    Nothing ->
                        []
               )
            ++ (case styles.fontSize of
                    Just size ->
                        [ ( "font-size", size ) ]

                    Nothing ->
                        []
               )
            ++ (case styles.backgroundImage of
                    Just { image, position } ->
                        [ ( "background-image", "url(" ++ image ++ ")" )
                        , ( "background-repeat", "no-repeat" )
                        , ( "background-position", position )
                        ]

                    Nothing ->
                        []
               )
            ++ (case styles.border of
                    Just color ->
                        [ ( "border", "1px solid " ++ toHash color ) ]

                    Nothing ->
                        []
               )
            ++ (if styles.hideOutline then
                    [ ( "outline", "none" ) ]

                else
                    []
               )


layoutAttr : Layout -> Html.Attribute msg
layoutAttr layout =
    style <|
        (case layout.height of
            Just h ->
                [ ( "height", h ) ]

            Nothing ->
                []
        )
            ++ (case layout.width of
                    Just w ->
                        [ ( "width", w ) ]

                    Nothing ->
                        []
               )
            ++ (case layout.layoutType of
                    Column ->
                        []

                    Row NoJustification ->
                        [ ( "display", "flex" ) ]

                    Row Spread ->
                        [ ( "display", "flex" )
                        , ( "justify-content", "space-between" )
                        ]
               )
            ++ (case layout.margin of
                    Just m ->
                        [ ( "margin", m ) ]

                    Nothing ->
                        []
               )
            ++ (if layout.anchorDescendants then
                    [ ( "position", "relative" ) ]

                else
                    []
               )
            ++ (case layout.padding of
                    Just p ->
                        [ ( "padding", p ) ]

                    Nothing ->
                        []
               )


viewFunc : NodeType -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
viewFunc nt =
    case nt of
        Default ->
            Html.div

        Button ->
            Html.button

        TextInput placeholderText ->
            \atts -> Html.input <| placeholder placeholderText :: atts


idAttr : Identifier -> List (Html.Attribute msg)
idAttr identifier =
    case identifier of
        Id iden ->
            [ id <| idToString iden ]

        Class cls ->
            [ class <| classToString cls ]

        Unidentified ->
            []


idToString : Id -> String
idToString id =
    case id of
        Page ->
            "page"

        TopBar ->
            "top-bar"

        ConcourseLogo ->
            "concourse-logo"

        SearchInput ->
            "search-input"


classToString : Class -> String
classToString _ =
    "class"


view : Node -> Html ()
view (Node identifier nt styles layout children) =
    viewFunc nt
        (idAttr identifier ++ [ styleAttr styles, layoutAttr layout ])
    <|
        List.map view children



-- SAMPLE APP


main : Program Never Node ()
main =
    Html.program
        { init = ( page, Cmd.none )
        , update = always <| flip (,) Cmd.none
        , subscriptions = always Sub.none
        , view = Debug.log "page" >> view
        }
