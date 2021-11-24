# Elm Placeholder

This library exists as an typesafe alternative to String interpolation via Regex.
Strings are parsed into functions that interpolate arguments at the specified positions, which means that contrary to the Regex approach,
parsing will fail if there is an unexpected amount of declared placeholders.

## Syntax

At the moment, this module supports interpolation via {{double curly bracket}} syntax.
The internals needed to support a different syntax are exposed.
