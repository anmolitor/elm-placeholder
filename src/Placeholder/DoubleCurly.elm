module Placeholder.DoubleCurly exposing
    ( parsePlaceholder1, parsePlaceholder2, parsePlaceholder3, parsePlaceholder4
    , parsePlaceholderAlph1, parsePlaceholderAlph2, parsePlaceholderAlph3, parsePlaceholderAlph4
    )

{-|


# DoubleCurly

Parse Strings into functions using the {{double curly brackets}} placeholder syntax.
For more detail, view the documentation in `Placeholder.Internal`.

@docs parsePlaceholder1, parsePlaceholder2, parsePlaceholder3, parsePlaceholder4
@docs parsePlaceholderAlph1, parsePlaceholderAlph2, parsePlaceholderAlph3, parsePlaceholderAlph4

-}

import Placeholder.Internal as Internal


syntax : Internal.Syntax
syntax =
    { startSymbol = "{{", endSymbol = "}}" }


{-| Parse a `String` into a function substituting the argument at the position marked by double curly braces.

    parsePlaceholder1 "This is {{name}}s example" |> Result.map ((|>) "Andy")
    --> Ok "This is Andys example"

-}
parsePlaceholder1 : String -> Result String (String -> String)
parsePlaceholder1 =
    Internal.parsePlaceholder1 syntax


{-| Parse a `String` into a function substituting the arguments at the two positions marked by double curly braces.
-}
parsePlaceholder2 : String -> Result String (String -> String -> String)
parsePlaceholder2 =
    Internal.parsePlaceholder2 syntax


{-| Parse a `String` into a function substituting the arguments at the three positions marked by double curly braces.
-}
parsePlaceholder3 : String -> Result String (String -> String -> String -> String)
parsePlaceholder3 =
    Internal.parsePlaceholder3 syntax


{-| Parse a `String` into a function substituting the arguments at the four positions marked by double curly braces.
-}
parsePlaceholder4 : String -> Result String (String -> String -> String -> String -> String)
parsePlaceholder4 =
    Internal.parsePlaceholder4 syntax


{-| Parse a `String` into a function substituting the argument at all positions marked by a key inside of double curly braces.

    parsePlaceholderAlph1 "This is {{name}}s example (made by {{name}})" |> Result.map ((|>) "Andy")
    --> Ok "This is Andys example (made by Andy)"

-}
parsePlaceholderAlph1 : String -> Result String (String -> String)
parsePlaceholderAlph1 =
    Internal.parsePlaceholderAlph1 syntax


{-| Parse a `String` into a function substituting the argument at all positions marked by two keys inside of double curly braces.
The arguments are substituted in alphabetical order of the keys.

    parsePlaceholderAlph2 "{{b}} wrote a letter to {{a}}, then {{a}} wrote one back to {{b}}" |> Result.map (\f -> f "Joe" "Mary")
    --> Ok "Mary wrote a letter to Joe, then Joe wrote one back to Mary"

-}
parsePlaceholderAlph2 : String -> Result String (String -> String -> String)
parsePlaceholderAlph2 =
    Internal.parsePlaceholderAlph2 syntax


{-| Parse a `String` into a function substituting the argument at all positions marked by three keys inside of double curly braces.
The arguments are substituted in alphabetical order of the keys.
-}
parsePlaceholderAlph3 : String -> Result String (String -> String -> String -> String)
parsePlaceholderAlph3 =
    Internal.parsePlaceholderAlph3 syntax


{-| Parse a `String` into a function substituting the argument at all positions marked by four keys inside of double curly braces.
The arguments are substituted in alphabetical order of the keys.
-}
parsePlaceholderAlph4 : String -> Result String (String -> String -> String -> String -> String)
parsePlaceholderAlph4 =
    Internal.parsePlaceholderAlph4 syntax
