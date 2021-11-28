module Placeholder.Internal exposing
    ( Template, parseTemplate, getPlaceholderNames, getAlphabeticalPlaceholderNames, templateToString, mapPlaceholders
    , Syntax, parsePlaceholder1, parsePlaceholder2, parsePlaceholder3, parsePlaceholder4
    , parsePlaceholderAlph1, parsePlaceholderAlph2, parsePlaceholderAlph3, parsePlaceholderAlph4
    )

{-| This module allows defining placeholder parsers based on a non-supported `Syntax` as well as
using the internal `Template` type for different purposes.


# Template

@docs Template, parseTemplate, getPlaceholderNames, getAlphabeticalPlaceholderNames, templateToString, mapPlaceholders


# Syntax

@docs Syntax, parsePlaceholder1, parsePlaceholder2, parsePlaceholder3, parsePlaceholder4
@docs parsePlaceholderAlph1, parsePlaceholderAlph2, parsePlaceholderAlph3, parsePlaceholderAlph4

-}

import List.NonEmpty exposing (NonEmpty)
import Parser exposing ((|.), (|=))


{-| A `Syntax` tells the `templateParser` how to distinguish placeholders from regular text.
At the moment, this can only be configured by setting start and end symbol.
-}
type alias Syntax =
    { startSymbol : String
    , endSymbol : String
    }


{-| A `Template` includes an ordered list of placeholders and text segments.
`Template`s guarantee that

    List.length segments == List.length placeholders + 1

and they can be converted back to the same `String` representation they were parsed from.

-}
type Template
    = Template TemplateInternal


type alias TemplateInternal =
    { placeholders : List ( List Int, String )
    , segments : NonEmpty String
    , syntax : Syntax
    , holes : Int
    }


emptyTemplate : Syntax -> String -> TemplateInternal
emptyTemplate syntax begin =
    { placeholders = [], segments = List.NonEmpty.singleton begin, syntax = syntax, holes = 0 }


addPlaceholder : String -> TemplateInternal -> TemplateInternal
addPlaceholder placeholder template =
    let
        helper : List ( List Int, String ) -> List ( List Int, String )
        helper list =
            case list of
                [] ->
                    [ ( [ template.holes ], placeholder ) ]

                (( pos, ph ) as head) :: rest ->
                    case compare placeholder ph of
                        LT ->
                            ( [ template.holes ], placeholder ) :: list

                        EQ ->
                            ( template.holes :: pos, ph ) :: rest

                        GT ->
                            head :: helper rest
    in
    { template | placeholders = helper template.placeholders, holes = template.holes + 1 }


addTextSegment : String -> TemplateInternal -> TemplateInternal
addTextSegment text template =
    { template | segments = List.NonEmpty.cons text template.segments }


templateParser : Syntax -> Parser.Parser TemplateInternal
templateParser ({ startSymbol, endSymbol } as syntax) =
    Parser.chompUntilEndOr startSymbol
        |> Parser.getChompedString
        |> Parser.andThen
            (\begin ->
                Parser.oneOf
                    [ Parser.succeed (emptyTemplate syntax begin) |. Parser.end
                    , Parser.succeed (\placeholder -> addPlaceholder placeholder >> addTextSegment begin)
                        |. Parser.token startSymbol
                        |= Parser.getChompedString (Parser.chompUntil endSymbol)
                        |. Parser.token endSymbol
                        |= Parser.lazy (\_ -> templateParser syntax)
                    ]
            )


{-| Parse a `String` into a `Template` based on a `Syntax`.

    syntax : Syntax
    syntax = { startSymbol = "${", endSymbol = "}" }

    parseTemplate syntax "This is ${name}s example" |> Result.map (always ())

    --> Ok ()
    parseTemplate syntax "This does not ${makeSense"

    --> Err "TODO deadEndsToString" -- (Waiting for upstream fix in elm/parser)

-}
parseTemplate : Syntax -> String -> Result String Template
parseTemplate syntax =
    Parser.run (templateParser syntax)
        >> Result.mapError Parser.deadEndsToString
        >> Result.map Template


{-| Get the names of the placeholders in a `Template`.
The names are ordered just like they are in the given template string.

    parseTemplate { startSymbol = "${", endSymbol = "}" } "${first} ${snd}"
        |> Result.map getPlaceholderNames
    --> Ok ["first", "snd"]

    parseTemplate { startSymbol = "${", endSymbol = "}" } "${blub} ${bla} ${blub}"
        |> Result.map getPlaceholderNames
    --> Ok ["blub", "bla", "blub"]

-}
getPlaceholderNames : Template -> List String
getPlaceholderNames (Template template) =
    List.concatMap (\( indices, str ) -> List.map (Tuple.pair str) indices) template.placeholders
        |> List.sortWith (\( _, a1 ) ( _, a2 ) -> compare a2 a1)
        |> List.map Tuple.first


{-| Get the names of the placeholders in a `Template`, but sorted in alphabetical order and without duplicates.

    parseTemplate { startSymbol = "${", endSymbol = "}" } "${blub} ${bla} ${blub}"
        |> Result.map getAlphabeticalPlaceholderNames
    --> Ok ["bla", "blub"]

-}
getAlphabeticalPlaceholderNames : Template -> List String
getAlphabeticalPlaceholderNames (Template template) =
    template.placeholders |> List.map Tuple.second


{-| Convert a parsed `Template` back to its `String` representation.
For any template built with `Syntax` s,

    templateToString >> parseTemplate s == identity

    parseTemplate { startSymbol = "${", endSymbol = "}" } "This is ${name}s example"
        |> Result.map templateToString
    --> Ok "This is ${name}s example"

-}
templateToString : Template -> String
templateToString ((Template { placeholders, segments, syntax }) as template) =
    List.NonEmpty.head segments
        :: List.map2 (\p s -> p ++ syntax.endSymbol ++ s) (getPlaceholderNames template) (List.NonEmpty.tail segments)
        |> String.join syntax.startSymbol


{-| Parse a `String` into a function substituting the argument at the position marked by the `Syntax`.

    exampleOf : Result String (String -> String)
    exampleOf = parsePlaceholder1 { startSymbol = "${", endSymbol = "}" } "This is ${name}s example"

    Result.map ((|>) "Andy") exampleOf
    --> Ok "This is Andys example"

    parsePlaceholder1 { startSymbol = "${", endSymbol = "}" } "No placeholder"
    --> Err "Expected 1 placeholders, but parsed a total of 0."

    parsePlaceholder1 { startSymbol = "${", endSymbol = "}" } "${multiple}${placeholders}"
    --> Err "Expected 1 placeholders, but parsed a total of 2."

-}
parsePlaceholder1 : Syntax -> String -> Result String (F1 String)
parsePlaceholder1 syntax =
    parseTemplate syntax
        >> Result.andThen (expectHoles 1)
        >> Result.map (\template -> l1 >> fillUnsafe template)


{-| Parse a `String` into a function substituting the arguments at the two positions marked by the `Syntax`.
Note that this function and the other `parsePlaceholderN` functions _always_ substitute elements in the order
they appear in the template string. This also means that for duplicate placeholder keys, you need to use
the `parsePlaceholderAlphN` family of functions instead.

    parsePlaceholder2 { startSymbol = "{", endSymbol = "}" } "{greeting} {target}"
        |> Result.map (\f -> f "Hello" "World")
    --> Ok "Hello World"

-}
parsePlaceholder2 : Syntax -> String -> Result String (F2 String)
parsePlaceholder2 syntax =
    parseTemplate syntax
        >> Result.andThen (expectHoles 2)
        >> Result.map (\template v1 -> l2 v1 >> fillUnsafe template)


{-| Parse a `String` into a function substituting the arguments at the three positions marked by the `Syntax`.
-}
parsePlaceholder3 : Syntax -> String -> Result String (F3 String)
parsePlaceholder3 syntax =
    parseTemplate syntax
        >> Result.andThen (expectHoles 3)
        >> Result.map (\template v1 v2 -> l3 v1 v2 >> fillUnsafe template)


{-| Parse a `String` into a function substituting the arguments at the four positions marked by the `Syntax`.
-}
parsePlaceholder4 : Syntax -> String -> Result String (F4 String)
parsePlaceholder4 syntax =
    parseTemplate syntax
        >> Result.andThen (expectHoles 4)
        >> Result.map (\template v1 v2 v3 -> l4 v1 v2 v3 >> fillUnsafe template)


{-| Parse a `String` into a function substituting the argument at all positions marked with the same, only key
This differs from `parsePlaceholder1` as you can see in the following example:

    syntax : Syntax
    syntax = { startSymbol = "{", endSymbol = "}" }

    parsePlaceholder1 syntax "{greeting} {greeting}"
    --> Err "Expected 1 placeholders, but parsed a total of 2."

    parsePlaceholderAlph1 syntax "{greeting} {greeting}" |> Result.map ((|>) "Hello")
    --> Ok "Hello Hello"

-}
parsePlaceholderAlph1 : Syntax -> String -> Result String (F1 String)
parsePlaceholderAlph1 syntax =
    parseTemplate syntax
        >> Result.andThen (expectPlaceholderKeys 1)
        >> Result.map (\template -> l1 >> fillAlphabeticalUnsafe template)


{-| Parse a `String` into a function substituting the arguments at all positions marked with the two placeholder keys.
The arguments for the keys are expected in alphabetical order.
This differs from `parsePlaceholder2` as you can see in the following example:

    syntax : Syntax
    syntax = { startSymbol = "{", endSymbol = "}" }

    parsePlaceholder2 syntax "{greeting} {friend}" |> Result.map (\f -> f "Hello" "World")
    --> Ok "Hello World"

    parsePlaceholderAlph2 syntax "{greeting} {friend}" |> Result.map (\f -> f "Hello" "World")
    --> Ok "World Hello"

-}
parsePlaceholderAlph2 : Syntax -> String -> Result String (F2 String)
parsePlaceholderAlph2 syntax =
    parseTemplate syntax
        >> Result.andThen (expectPlaceholderKeys 2)
        >> Result.map (\template v1 -> l2 v1 >> fillAlphabeticalUnsafe template)


{-| Parse a `String` into a function substituting the arguments at all positions marked with the three placeholder keys.
The arguments for the keys are expected in alphabetical order.
-}
parsePlaceholderAlph3 : Syntax -> String -> Result String (F3 String)
parsePlaceholderAlph3 syntax =
    parseTemplate syntax
        >> Result.andThen (expectPlaceholderKeys 3)
        >> Result.map (\template v1 v2 -> l3 v1 v2 >> fillAlphabeticalUnsafe template)


{-| Parse a `String` into a function substituting the arguments at all positions marked with the four placeholder keys.
The arguments for the keys are expected in alphabetical order.
-}
parsePlaceholderAlph4 : Syntax -> String -> Result String (F4 String)
parsePlaceholderAlph4 syntax =
    parseTemplate syntax
        >> Result.andThen (expectPlaceholderKeys 4)
        >> Result.map (\template v1 v2 v3 -> l4 v1 v2 v3 >> fillAlphabeticalUnsafe template)


{-| Change placeholder keys based on index and current key.

    parseTemplate { startSymbol = "{", endSymbol = "}" } "{UPPER} case"
        |> Result.map (mapPlaceholders (\_ -> String.toLower) >> templateToString)
    --> Ok "{upper} case"

-}
mapPlaceholders : (Int -> String -> String) -> Template -> Template
mapPlaceholders f (Template template) =
    Template
        { template
            | placeholders =
                List.indexedMap
                    (\i ( indices, key ) ->
                        ( indices, f i key )
                    )
                    template.placeholders
        }


expectHoles : Int -> Template -> Result String Template
expectHoles expectedHoles ((Template { holes }) as template) =
    if expectedHoles == holes then
        Ok template

    else
        Err <|
            "Expected "
                ++ String.fromInt expectedHoles
                ++ " placeholders, but parsed a total of "
                ++ String.fromInt holes
                ++ "."


expectPlaceholderKeys : Int -> Template -> Result String Template
expectPlaceholderKeys expectedNumberOfKeys ((Template { placeholders }) as template) =
    if expectedNumberOfKeys == List.length placeholders then
        Ok template

    else
        Err <|
            "Expected "
                ++ String.fromInt expectedNumberOfKeys
                ++ " placeholder keys, but parsed ["
                ++ (List.map Tuple.second placeholders |> String.join ", ")
                ++ "]"


fillUnsafe : Template -> List String -> String
fillUnsafe (Template { segments, placeholders }) values =
    List.NonEmpty.head segments
        :: List.map2 (++) values (List.NonEmpty.tail segments)
        |> String.join ""


fillAlphabeticalUnsafe : Template -> List String -> String
fillAlphabeticalUnsafe (Template { segments, placeholders }) values =
    let
        valuesInCorrectOrder =
            List.map2 (\( indices, _ ) value -> List.map (Tuple.pair value) indices) placeholders values
                |> List.concat
                |> List.sortWith (\( _, a1 ) ( _, a2 ) -> compare a2 a1)
                |> List.map Tuple.first
    in
    List.NonEmpty.head segments
        :: List.map2 (++) valuesInCorrectOrder (List.NonEmpty.tail segments)
        |> String.join ""


type alias F1 a =
    a -> a


type alias F2 a =
    a -> F1 a


type alias F3 a =
    a -> F2 a


type alias F4 a =
    a -> F3 a


type alias F5 a =
    a -> F4 a


type alias F6 a =
    a -> F5 a


type alias F7 a =
    a -> F6 a


type alias F8 a =
    a -> F7 a


type alias F9 a =
    a -> F8 a


l1 : String -> List String
l1 =
    List.singleton


l2 : String -> String -> List String
l2 p1 p2 =
    [ p1, p2 ]


l3 : String -> String -> String -> List String
l3 p1 p2 p3 =
    [ p1, p2, p3 ]


l4 : String -> String -> String -> String -> List String
l4 p1 p2 p3 p4 =
    [ p1, p2, p3, p4 ]
