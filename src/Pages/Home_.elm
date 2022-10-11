module Pages.Home_ exposing (Model, Msg, page)

import Array exposing (Array)
import Combinatorics exposing (getCombinationsRepeating, getVariations)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Gen.Params.Home_ exposing (Params)
import List.Extra
import Page
import Parser exposing (..)
import Request
import Set
import Shared
import View exposing (View)



-- Dimensions


numFiles : Int
numFiles =
    7


numRanks : Int
numRanks =
    16


numTiles : Int
numTiles =
    numFiles * numRanks


type PieceKind
    = Attacker
    | Defender
    | Rusher
    | Spy
    | Sentry
    | Gem
    | Wall


pieceKindToString : PieceKind -> String
pieceKindToString kind =
    case kind of
        Attacker ->
            "Attacker"

        Defender ->
            "Defender"

        Rusher ->
            "Rusher"

        Spy ->
            "Spy"

        Sentry ->
            "Sentry"

        Gem ->
            "Gem"

        Wall ->
            "Wall"


type Color
    = Red
    | Blue


type TileKind
    = Blank
    | Occupied ( Color, PieceKind )
    | OutOfBounds


tileKindToMaybe : TileKind -> Maybe ( Color, PieceKind )
tileKindToMaybe kind =
    case kind of
        Occupied x ->
            Just x

        _ ->
            Nothing


type alias Tile =
    { kind : TileKind
    }


type alias Board =
    { tiles : Array Tile
    }


type SpecialAction
    = None
    | CaptureGem
    | CapturePlayer
    | WinGem


type Turn
    = Move SpecialAction Pos Pos
    | Spawn Pos ( Color, PieceKind )


colorParser : Parser Color
colorParser =
    oneOf
        [ succeed Red |. symbol "r"
        , succeed Blue |. symbol "b"
        ]


validMoves : Tile -> Pos -> Board -> List ( SpecialAction, Pos, Pos )
validMoves tile src board =
    case tile.kind of
        Blank ->
            []

        OutOfBounds ->
            []

        Occupied ( color, kind ) ->
            validMovesInner color kind src board


rusherOffsets : List ( Int, Int )
rusherOffsets =
    Combinatorics.getCombinationsRepeating 4 Combinatorics.offsets
        |> List.map
            (\inner ->
                List.foldl
                    (\( a1, b1 ) ( a2, b2 ) -> ( a1 + a2, b1 + b2 ))
                    ( 0, 0 )
                    inner
            )
        |> Set.fromList
        |> Set.toList


validMovesInner : Color -> PieceKind -> Pos -> Board -> List ( SpecialAction, Pos, Pos )
validMovesInner who kind src board =
    let
        checkNotWallOrSameGem dest =
            let
                tile =
                    getTile dest board
            in
            case tile.kind of
                OutOfBounds ->
                    Nothing

                Occupied ( colour, tkind ) ->
                    if colour == who && tkind == Gem then
                        Nothing

                    else if tkind == Gem then
                        Just ( CaptureGem, src, dest )

                    else if tkind == Wall then
                        Nothing

                    else if colour /= who then
                        Just ( CapturePlayer, src, dest )

                    else
                        Nothing

                Blank ->
                    Just ( None, src, dest )
    in
    case kind of
        Defender ->
            Combinatorics.offsets
                |> List.map (\( x, y ) -> offsetPos x y src)
                |> List.map (Maybe.andThen checkNotWallOrSameGem)
                |> List.filterMap identity

        Spy ->
            Combinatorics.offsets
                |> List.map (\( x, y ) -> offsetPos x y src)
                |> List.map (Maybe.andThen checkNotWallOrSameGem)
                |> List.filterMap identity

        Sentry ->
            (Combinatorics.offsets ++ Combinatorics.offsets2)
                |> List.map (\( x, y ) -> offsetPos x y src)
                |> List.map (Maybe.andThen checkNotWallOrSameGem)
                |> List.filterMap identity

        Attacker ->
            (Combinatorics.offsets ++ Combinatorics.offsets2 ++ Combinatorics.offsets3)
                |> List.map (\( x, y ) -> offsetPos x y src)
                |> List.map (Maybe.andThen checkNotWallOrSameGem)
                |> List.filterMap identity

        Rusher ->
            rusherOffsets
                |> List.map (\( x, y ) -> offsetPos x y src)
                |> List.map (Maybe.andThen checkNotWallOrSameGem)
                |> List.filterMap identity

        _ ->
            []


kindParser : Parser PieceKind
kindParser =
    oneOf
        [ succeed Attacker |. symbol "a"
        , succeed Defender |. symbol "d"
        , succeed Rusher |. symbol "r"
        , succeed Spy |. symbol "s"
        , succeed Sentry |. symbol "t"
        , succeed Gem |. symbol "g"
        , succeed Wall |. symbol "w"
        ]


turnParser : Parser Turn
turnParser =
    oneOf
        [ succeed Move
            |. symbol "m"
            |= oneOf
                [ succeed CapturePlayer |. symbol "x"
                , succeed CaptureGem |. symbol "g"
                , succeed WinGem |. symbol "w"
                , succeed None
                ]
            |= posParser
            |= posParser
        , succeed Spawn
            |. symbol "s"
            |= posParser
            |= (succeed Tuple.pair
                    |= colorParser
                    |= kindParser
               )
        ]


executeTurn : Turn -> Board -> Board
executeTurn move board =
    case move of
        Move spa from to ->
            let
                fromTile =
                    getTile from board
            in
            board
                |> setTile fromTile to
                |> setTile (Tile Blank) from

        Spawn pos ( colour, kind ) ->
            board
                |> setTile (makePiece colour kind) pos


type alias Pos =
    ( Rank, File )


chunksOfLeftList : Int -> List a -> List (List a)
chunksOfLeftList k xs =
    if k == 0 then
        [ [] ]

    else if k < 0 then
        []

    else if List.length xs > k then
        List.take k xs :: chunksOfLeftList k (List.drop k xs)

    else
        [ xs ]


makeEmptyBoard : Board
makeEmptyBoard =
    Board (Array.repeat numTiles (Tile Blank))


makePiece : Color -> PieceKind -> Tile
makePiece colour kind =
    Tile (Occupied ( colour, kind ))


makeStartingBoard : Board
makeStartingBoard =
    makeEmptyBoard
        -- middle elements
        |> setTilePiece Blue Gem C Four
        |> setTilePiece Blue Wall F Four
        |> setTilePiece Red Gem J Four
        |> setTilePiece Red Wall G Four
        -- blue far elements
        |> setTilePiece Blue Defender PrePreA Six
        |> setTilePiece Blue Defender PrePreA Four
        |> setTilePiece Blue Defender PrePreA Two
        -- blue elements
        |> setTilePiece Blue Spy PreA Seven
        |> setTilePiece Blue Rusher PreA Six
        |> setTilePiece Blue Attacker PreA Four
        |> setTilePiece Blue Rusher PreA Two
        |> setTilePiece Blue Sentry PreA One
        -- red far elements
        |> setTilePiece Red Defender PostPostL Six
        |> setTilePiece Red Defender PostPostL Four
        |> setTilePiece Red Defender PostPostL Two
        -- red elements
        |> setTilePiece Red Spy PostL Seven
        |> setTilePiece Red Rusher PostL Six
        |> setTilePiece Red Attacker PostL Four
        |> setTilePiece Red Rusher PostL Two
        |> setTilePiece Red Sentry PostL One


getTile : Pos -> Board -> Tile
getTile pos board =
    Array.get (posToIndex pos) board.tiles
        |> Maybe.withDefault (Tile OutOfBounds)


setTile : Tile -> Pos -> Board -> Board
setTile tile pos board =
    { board
        | tiles = Array.set (posToIndex pos) tile board.tiles
    }


setTilePiece : Color -> PieceKind -> Rank -> File -> Board -> Board
setTilePiece colour kind rank file =
    setTile (makePiece colour kind) ( rank, file )


type Rank
    = PrePreA
    | PreA
    | A
    | B
    | C
    | D
    | E
    | F
    | G
    | H
    | I
    | J
    | K
    | L
    | PostL
    | PostPostL


posToIndex : Pos -> Int
posToIndex ( rank, file ) =
    rankToIndex rank + (fileToIndex file * numRanks)


indexesToPos : ( Int, Int ) -> Maybe Pos
indexesToPos ( rank, file ) =
    Maybe.map2 Tuple.pair (indexToRank rank) (indexToFile file)


offsetPos : Int -> Int -> ( Rank, File ) -> Maybe Pos
offsetPos dR dF ( rank, file ) =
    indexesToPos ( rankToIndex rank + dR, fileToIndex file + dF )


boardIndexToPos : Int -> Maybe Pos
boardIndexToPos index =
    indexesToPos ( remainderBy numRanks index, index // numRanks )


indexToFile : Int -> Maybe File
indexToFile index =
    case index of
        0 ->
            Just Seven

        1 ->
            Just Six

        2 ->
            Just Five

        3 ->
            Just Four

        4 ->
            Just Three

        5 ->
            Just Two

        6 ->
            Just One

        _ ->
            Nothing


indexToRank : Int -> Maybe Rank
indexToRank index =
    case index of
        0 ->
            Just PrePreA

        1 ->
            Just PreA

        2 ->
            Just A

        3 ->
            Just B

        4 ->
            Just C

        5 ->
            Just D

        6 ->
            Just E

        7 ->
            Just F

        8 ->
            Just G

        9 ->
            Just H

        10 ->
            Just I

        11 ->
            Just J

        12 ->
            Just K

        13 ->
            Just L

        14 ->
            Just PostL

        15 ->
            Just PostPostL

        _ ->
            Nothing


rankToIndex : Rank -> Int
rankToIndex rank =
    case rank of
        PrePreA ->
            0

        PreA ->
            1

        A ->
            2

        B ->
            3

        C ->
            4

        D ->
            5

        E ->
            6

        F ->
            7

        G ->
            8

        H ->
            9

        I ->
            10

        J ->
            11

        K ->
            12

        L ->
            13

        PostL ->
            14

        PostPostL ->
            15


rankParser : Parser Rank
rankParser =
    oneOf
        [ succeed identity
            |. symbol "b"
            |= oneOf
                [ succeed PrePreA |. symbol "-"
                , succeed PreA |. symbol "+"
                ]
        , succeed identity
            |. symbol "r"
            |= oneOf
                [ succeed PostPostL |. symbol "-"
                , succeed PostL |. symbol "+"
                ]
        , succeed A |. symbol "A"
        , succeed B |. symbol "B"
        , succeed C |. symbol "C"
        , succeed D |. symbol "D"
        , succeed E |. symbol "E"
        , succeed F |. symbol "F"
        , succeed G |. symbol "G"
        , succeed H |. symbol "H"
        , succeed I |. symbol "I"
        , succeed J |. symbol "J"
        , succeed K |. symbol "K"
        , succeed L |. symbol "L"
        ]


fileParser : Parser File
fileParser =
    oneOf
        [ succeed Seven |. symbol "7"
        , succeed Six |. symbol "6"
        , succeed Five |. symbol "5"
        , succeed Four |. symbol "4"
        , succeed Three |. symbol "3"
        , succeed Two |. symbol "2"
        , succeed One |. symbol "1"
        ]


posParser : Parser Pos
posParser =
    succeed Tuple.pair
        |= rankParser
        |= fileParser


type File
    = Seven
    | Six
    | Five
    | Four
    | Three
    | Two
    | One


fileToIndex : File -> Int
fileToIndex file =
    case file of
        Seven ->
            0

        Six ->
            1

        Five ->
            2

        Four ->
            3

        Three ->
            4

        Two ->
            5

        One ->
            6


renderPiece : ( Pos, Tile ) -> Maybe Turn -> Element Msg
renderPiece ( pos, tile ) turn =
    let
        highlight x =
            el
                [ width (px 64)
                , height (px 64)
                , Background.color <| rgba255 0x00 0xFF 0x00 0.3
                , Border.width 2
                , Border.color <| rgb255 0x00 0xFF 0x00
                , onClick <| MakeTurn x
                ]
                none

        extraAttrs =
            (case turn of
                Just it ->
                    [ inFront (highlight it) ]

                Nothing ->
                    []
            )
                ++ [ onClick (ViewPossible ( tile, pos )) ]
    in
    case tile.kind of
        OutOfBounds ->
            none

        Blank ->
            el
                ([ width (px 64)
                 , height (px 64)
                 , Border.width 2
                 , Border.color <| rgb255 0xEE 0xEE 0xEE
                 ]
                    ++ extraAttrs
                )
                none

        Occupied ( color, kind ) ->
            let
                borColor =
                    case color of
                        Red ->
                            rgb255 0xFF 0x00 0x00

                        Blue ->
                            rgb255 0x00 0x00 0xFF

                bacColor =
                    case color of
                        Red ->
                            rgba255 0xFF 0x00 0x00 0.1

                        Blue ->
                            rgba255 0x00 0x00 0xFF 0.1
            in
            el
                ([ width (px 64)
                 , height (px 64)
                 , Border.width 2
                 , Border.color borColor
                 , Background.color bacColor
                 ]
                    ++ extraAttrs
                )
                (el [ centerX, centerY, Font.size 14 ] (text (pieceKindToString kind)))


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { board : Board
    , input : String
    , viewing : Maybe ( Tile, Pos )
    }


init : ( Model, Cmd Msg )
init =
    ( Model makeStartingBoard "" Nothing, Cmd.none )



-- UPDATE


type Msg
    = InputEdited String
    | ExecuteInput
    | ViewPossible ( Tile, Pos )
    | MakeTurn Turn


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputEdited txt ->
            ( { model | input = txt }, Cmd.none )

        ExecuteInput ->
            let
                res =
                    Parser.run turnParser model.input
            in
            case res of
                Ok turn ->
                    ( { model | board = executeTurn turn model.board }, Cmd.none )

                Err err ->
                    ( model, Cmd.none )

        ViewPossible pos ->
            ( { model | viewing = Just pos }, Cmd.none )

        MakeTurn turn ->
            ( { model | board = executeTurn turn model.board }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


deadEndsToString : List DeadEnd -> String
deadEndsToString deadEnds =
    String.concat (List.intersperse "; " (List.map deadEndToString deadEnds))


deadEndToString : DeadEnd -> String
deadEndToString deadend =
    problemToString deadend.problem ++ " at row " ++ String.fromInt deadend.row ++ ", col " ++ String.fromInt deadend.col


problemToString : Problem -> String
problemToString p =
    case p of
        Expecting s ->
            "expecting '" ++ s ++ "'"

        ExpectingInt ->
            "expecting int"

        ExpectingHex ->
            "expecting hex"

        ExpectingOctal ->
            "expecting octal"

        ExpectingBinary ->
            "expecting binary"

        ExpectingFloat ->
            "expecting float"

        ExpectingNumber ->
            "expecting number"

        ExpectingVariable ->
            "expecting variable"

        ExpectingSymbol s ->
            "expecting symbol '" ++ s ++ "'"

        ExpectingKeyword s ->
            "expecting keyword '" ++ s ++ "'"

        ExpectingEnd ->
            "expecting end"

        UnexpectedChar ->
            "unexpected char"

        Problem s ->
            "problem " ++ s

        BadRepeat ->
            "bad repeat"


view : Model -> View Msg
view model =
    let
        viewings =
            model.viewing
                |> Maybe.map (\( t, p ) -> validMoves t p model.board)
                |> Maybe.withDefault []

        predicate : Pos -> ( SpecialAction, Pos, Pos ) -> Bool
        predicate self ( _, _, dest ) =
            self == dest

        rendered =
            Array.toIndexedList model.board.tiles
                |> List.map (Tuple.mapFirst boardIndexToPos)
                |> List.map (\( m, o ) -> Maybe.map (Tuple.pair o) m)
                |> List.filterMap identity
                |> List.map (\( a, b ) -> ( b, a ))
                |> chunksOfLeftList numRanks
                |> List.map
                    (List.map
                        (\( p, t ) ->
                            renderPiece ( p, t )
                                (List.Extra.find (predicate p) viewings
                                    |> Maybe.map
                                        (\( spa, p1, p2 ) -> Move spa p1 p2)
                                )
                        )
                    )
                |> List.map (row [])
                |> column [ Border.width 2, Border.color <| rgb255 0xEE 0xEE 0xEE ]
    in
    { title = "Homepage"
    , body =
        column [ centerX, centerY ]
            [ el [] rendered
            , Input.text [] { onChange = InputEdited, text = model.input, placeholder = Nothing, label = Input.labelHidden "gaming" }
            , Input.button [] { onPress = Just ExecuteInput, label = text "Run" }
            , let
                res =
                    Parser.run turnParser model.input
              in
              case res of
                Ok _ ->
                    none

                Err err ->
                    paragraph [] [ text (deadEndsToString err) ]
            ]
    }
