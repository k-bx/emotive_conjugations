module EnumeratedList exposing
    ( EnumeratedItem
    , EnumeratedList(..)
    , deleteAtItem
    , findByPosition
    , fromList
    , length
    , map
    , toList
    , updateAtItem
    , updateAtItemPosition
    , updateValAtItem
    )

{-| List with enumerated elements.

Abstraction very similar to a list but enumerating items with a
numeric index, emulating an "id" per each record.
-}

import List
import List.Extra


type alias EnumeratedItem a =
    { position : Int
    , element : a
    }


type EnumeratedList a
    = EnumeratedList (List (EnumeratedItem a))


fromList : List a -> EnumeratedList a
fromList =
    EnumeratedList << List.indexedMap (\i x -> { position = i, element = x })


toList : EnumeratedList a -> List a
toList (EnumeratedList xs) =
    List.map .element xs


map : (EnumeratedItem a -> b) -> EnumeratedList a -> EnumeratedList b
map f (EnumeratedList xs) =
    fromList <|
        List.map (\item -> f item) xs


findByPosition : Int -> EnumeratedList a -> Maybe (EnumeratedItem a)
findByPosition n (EnumeratedList xs) =
    List.Extra.getAt n xs


updateAtItem :
    EnumeratedItem a
    -> (EnumeratedItem a -> Maybe a)
    -> EnumeratedList a
    -> EnumeratedList a
updateAtItem item f (EnumeratedList xs) =
    let
        go i yss =
            case yss of
                [] ->
                    []

                y :: ys ->
                    if i == item.position then
                        case f y of
                            Nothing ->
                                List.map .element ys

                            Just y2 ->
                                y2 :: List.map .element ys

                    else
                        y.element :: go (i + 1) ys
    in
    fromList (go 0 xs)


deleteAtItem :
    EnumeratedItem a
    -> EnumeratedList a
    -> EnumeratedList a
deleteAtItem item xs =
    updateAtItem item (always Nothing) xs


updateValAtItem :
    EnumeratedItem a
    -> (a -> Maybe a)
    -> EnumeratedList a
    -> EnumeratedList a
updateValAtItem item f xs =
    updateAtItem item
        (\v ->
            f v.element
        )
        xs


updateAtItemPosition :
    Int
    -> (EnumeratedItem a -> Maybe a)
    -> EnumeratedList a
    -> EnumeratedList a
updateAtItemPosition position f (EnumeratedList xs) =
    let
        go i yss =
            case yss of
                [] ->
                    []

                y :: ys ->
                    if i == position then
                        case f y of
                            Nothing ->
                                List.map .element ys

                            Just y2 ->
                                y2 :: List.map .element ys

                    else
                        y.element :: go (i + 1) ys
    in
    fromList (go 0 xs)


length : EnumeratedList a -> Int
length =
    List.length << toList
