module Combinatorics exposing (..)

offsets : List (Int, Int)
offsets =
    [ (1, 0)
    , (-1, 0)
    , (0, 1)
    , (0, -1)
    , (1, 1)
    , (1, -1)
    , (-1, 1)
    , (-1, -1)
    ]

offsets2 : List (Int, Int)
offsets2 =
    [ (2, 0)
    , (-2, 0)
    , (0, 2)
    , (0, -2)
    , (2, 2)
    , (2, -2)
    , (-2, 2)
    , (-2, -2)
    ]

offsets3 : List (Int, Int)
offsets3 =
    [ (3, 0)
    , (-3, 0)
    , (0, 3)
    , (0, -3)
    , (3, 3)
    , (3, -3)
    , (-3, 3)
    , (-3, -3)
    ]

getVariations : Int -> List a -> List (List a)
getVariations k set =
    let
        doGetVariations ik iset depth resultItem =
            if depth < ik then
                iset
                    |> List.concatMap
                        (\setItem ->
                            if List.member setItem resultItem then
                                []

                            else
                                doGetVariations ik iset (depth + 1) (setItem :: resultItem)
                        )

            else
                [ resultItem |> List.reverse ]
    in
    doGetVariations k set 0 []


getPermutations : List a -> List (List a)
getPermutations set =
    getVariations (List.length set) set


getCombinationsRepeating : Int -> List a -> List (List a)
getCombinationsRepeating k set =
    if k == 0 then
        [ [] ]

    else
        case set of
            [] ->
                []

            x :: xs ->
                List.map ((::) x) (getCombinationsRepeating (k - 1) set) ++ getCombinationsRepeating k xs

getCombinationsNonRepeating : Int -> List a -> List (List a)
getCombinationsNonRepeating k set =
    if k == 0 then
        [ [] ]
    else
        case set of
            [] ->
                []

            x :: xs ->
                List.map ((::) x) (getCombinationsNonRepeating (k - 1) xs) ++ getCombinationsNonRepeating k xs