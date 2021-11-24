module Placeholder.Internal exposing
    ( Template, templateParser, parseTemplate
    , Syntax, parsePlaceholder1, parsePlaceholder2, parsePlaceholder3, parsePlaceholder4
    )

{-| This module allows defining placeholder parsers based on a non-supported `Syntax` as well as
using the internal `Template` type for different purposes.


# Template

@docs Template, templateParser, parseTemplate


# Syntax

@docs Syntax, parsePlaceholder1, parsePlaceholder2, parsePlaceholder3, parsePlaceholder4

-}

import Parser exposing ((|.), (|=))


{-| A `Syntax` tells the `templateParser` how to distinguish placeholders from regular text.
At the moment, this can only be configured by setting start and end symbol.
-}
type alias Syntax =
    { startSymbol : String
    , endSymbol : String
    }


{-| A `Template` includes an ordered list of placeholders and text segments.
`Template`s coming from this module guarantee that:

    > not (List.isEmpty segments)
    > List.length segments == List.length placeholders + 1

-}
type alias Template =
    { placeholders : List String
    , segments : List String
    }


emptyTemplate : String -> Template
emptyTemplate begin =
    { placeholders = [], segments = [ begin ] }


addPlaceholder : String -> Template -> Template
addPlaceholder placeholder template =
    { template | placeholders = placeholder :: template.placeholders }


addTextSegment : String -> Template -> Template
addTextSegment text template =
    { template | segments = text :: template.segments }


{-| A parser for a `Template` based on a `Syntax`.
In this form, it can be combined nicely with other `Parser`s.
-}
templateParser : Syntax -> Parser.Parser Template
templateParser ({ startSymbol, endSymbol } as syntax) =
    Parser.chompUntilEndOr startSymbol
        |> Parser.getChompedString
        |> Parser.andThen
            (\begin ->
                Parser.oneOf
                    [ Parser.succeed (emptyTemplate begin) |. Parser.end
                    , Parser.succeed (\placeholder -> addPlaceholder placeholder >> addTextSegment begin)
                        |. Parser.token startSymbol
                        |= Parser.getChompedString (Parser.chompUntil endSymbol)
                        |. Parser.token endSymbol
                        |= Parser.lazy (\_ -> templateParser syntax)
                    ]
            )


{-| Parse a `String` into a `Template` based on a `Syntax`.
Example:

    > parseTemplate { startSymbol = "${", endSymbol = "}" } "This is ${name}s example"
    { segments = ["This is ", "s example"], placeholders = ["name"]}

-}
parseTemplate : Syntax -> String -> Result String Template
parseTemplate syntax =
    Parser.run (templateParser syntax) >> Result.mapError Parser.deadEndsToString


{-| Parse a `String` into a function substituting the argument at the position marked by the `Syntax`.
Examples:

    > f = parsePlaceholder1 { startSymbol = "${", endSymbol = "}" } "This is ${name}s example"
    Ok <function> : Result String (F1 String)

    > Result.map ((|>) "Andy") f
    Ok ("This is Andys example") : Result String String

    > parsePlaceholder1 { startSymbol = "${", endSymbol = "}" } "No placeholder"
    Err ("Expected more placeholders.") : Result String (F1 String)

    > parsePlaceholder1 { startSymbol = "${", endSymbol = "}" } "${multiple}${placeholders}"
    Err ("Expected less placeholders.") : Result String (F1 String)

-}
parsePlaceholder1 : Syntax -> String -> Result String (F1 String)
parsePlaceholder1 syntax =
    parseTemplate syntax >> Result.andThen (\{ segments } -> start ( segments, f1 ) |> enough)


{-| Parse a `String` into a function substituting the arguments at the two positions marked by the `Syntax`.
-}
parsePlaceholder2 : Syntax -> String -> Result String (F2 String)
parsePlaceholder2 syntax =
    parseTemplate syntax >> Result.andThen (\{ segments } -> start ( segments, f2 ) |> oneMore |> enough)


{-| Parse a `String` into a function substituting the arguments at the three positions marked by the `Syntax`.
-}
parsePlaceholder3 : Syntax -> String -> Result String (F3 String)
parsePlaceholder3 syntax =
    parseTemplate syntax >> Result.andThen (\{ segments } -> start ( segments, f3 ) |> twoMore |> enough)


{-| Parse a `String` into a function substituting the arguments at the four positions marked by the `Syntax`.
-}
parsePlaceholder4 : Syntax -> String -> Result String (F4 String)
parsePlaceholder4 syntax =
    parseTemplate syntax >> Result.andThen (\{ segments } -> start ( segments, f4 ) |> twoMore |> oneMore |> enough)


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


f1 : F3 String
f1 s1 s2 p1 =
    s1 ++ p1 ++ s2


f2 : F5 String
f2 s1 s2 s3 p1 p2 =
    s1 ++ p1 ++ f1 s2 s3 p2


f3 : F7 String
f3 s1 s2 s3 s4 p1 p2 p3 =
    s1 ++ p1 ++ f2 s2 s3 s4 p2 p3


f4 : F9 String
f4 s1 s2 s3 s4 s5 p1 p2 p3 p4 =
    s1 ++ p1 ++ f3 s2 s3 s4 s5 p2 p3 p4


start : ( List String, String -> String -> b ) -> Result String ( List String, b )
start =
    Ok >> twoMore


oneMore : Result String ( List String, String -> b ) -> Result String ( List String, b )
oneMore =
    Result.andThen <|
        \( segments, f ) ->
            case segments of
                [] ->
                    Err "Expected more placeholders."

                first :: rest ->
                    Ok ( rest, f first )


twoMore : Result String ( List String, String -> String -> b ) -> Result String ( List String, b )
twoMore =
    oneMore >> oneMore


enough : Result String ( List String, b ) -> Result String b
enough =
    Result.andThen <|
        \( segments, b ) ->
            if List.isEmpty segments then
                Ok b

            else
                Err <| "Expected less placeholders."
