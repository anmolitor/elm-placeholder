module Placeholder.DoubleCurly exposing (parsePlaceholder1, parsePlaceholder2, parsePlaceholder3, parsePlaceholder4)

{-|


# DoubleCurly

Parse Strings into functions using the {{double curly brackets}} placeholder syntax.
For more detail, view the documentation in `Placeholder.Internal`.

@docs parsePlaceholder1, parsePlaceholder2, parsePlaceholder3, parsePlaceholder4

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
