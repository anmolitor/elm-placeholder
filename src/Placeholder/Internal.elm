module Placeholder.Internal exposing
    ( Template, parseTemplate, getPlaceholderNames, templateToString
    , Syntax, parsePlaceholder1, parsePlaceholder2, parsePlaceholder3, parsePlaceholder4
    , templateParser
    )

{-| This module allows defining placeholder parsers based on a non-supported `Syntax` as well as
using the internal `Template` type for different purposes.


# Template

@docs Template, parseTemplate, getPlaceholderNames, templateToString


# Syntax

@docs Syntax, parsePlaceholder1, parsePlaceholder2, parsePlaceholder3, parsePlaceholder4

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
    { placeholders : List String
    , segments : NonEmpty String
    , syntax : Syntax
    }


emptyTemplate : Syntax -> String -> TemplateInternal
emptyTemplate syntax begin =
    { placeholders = [], segments = List.NonEmpty.singleton begin, syntax = syntax }


addPlaceholder : String -> TemplateInternal -> TemplateInternal
addPlaceholder placeholder template =
    { template | placeholders = placeholder :: template.placeholders }


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

    parseTemplate { startSymbol = "${", endSymbol = "}" } "${first} ${snd}"
        |> Result.map getPlaceholderNames
    --> Ok ["first", "snd"]

-}
getPlaceholderNames : Template -> List String
getPlaceholderNames (Template template) =
    template.placeholders


{-| Convert a parsed `Template` back to its `String` representation.
For any template built with `Syntax` s,

    templateToString >> parseTemplate s == identity

    parseTemplate { startSymbol = "${", endSymbol = "}" } "This is ${name}s example"
        |> Result.map templateToString
    --> Ok "This is ${name}s example"

-}
templateToString : Template -> String
templateToString (Template { placeholders, segments, syntax }) =
    List.NonEmpty.head segments
        :: List.map2 (\p s -> p ++ syntax.endSymbol ++ s) placeholders (List.NonEmpty.tail segments)
        |> String.join syntax.startSymbol


{-| Parse a `String` into a function substituting the argument at the position marked by the `Syntax`.

    exampleOf : Result String (String -> String)
    exampleOf = parsePlaceholder1 { startSymbol = "${", endSymbol = "}" } "This is ${name}s example"

    Result.map ((|>) "Andy") exampleOf
    --> Ok "This is Andys example"

    parsePlaceholder1 { startSymbol = "${", endSymbol = "}" } "No placeholder"
    --> Err "Expected more placeholders."

    parsePlaceholder1 { startSymbol = "${", endSymbol = "}" } "${multiple}${placeholders}"
    --> Err "Expected less placeholders."

-}
parsePlaceholder1 : Syntax -> String -> Result String (F1 String)
parsePlaceholder1 syntax =
    parseTemplate syntax >> Result.andThen (start f1 >> enough)


{-| Parse a `String` into a function substituting the arguments at the two positions marked by the `Syntax`.

    parsePlaceholder2 { startSymbol = "{", endSymbol = "}" } "{greeting} {target}"
        |> Result.map (\f -> f "Hello" "World")
    --> Ok "Hello World"

-}
parsePlaceholder2 : Syntax -> String -> Result String (F2 String)
parsePlaceholder2 syntax =
    parseTemplate syntax >> Result.andThen (start f2 >> oneMore >> enough)


{-| Parse a `String` into a function substituting the arguments at the three positions marked by the `Syntax`.
-}
parsePlaceholder3 : Syntax -> String -> Result String (F3 String)
parsePlaceholder3 syntax =
    parseTemplate syntax >> Result.andThen (start f3 >> twoMore >> enough)


{-| Parse a `String` into a function substituting the arguments at the four positions marked by the `Syntax`.
-}
parsePlaceholder4 : Syntax -> String -> Result String (F4 String)
parsePlaceholder4 syntax =
    parseTemplate syntax >> Result.andThen (start f4 >> twoMore >> oneMore >> enough)


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


start : (String -> String -> b) -> Template -> Result String ( List String, b )
start f (Template { segments }) =
    ( List.NonEmpty.toList segments, f ) |> Ok >> twoMore


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
